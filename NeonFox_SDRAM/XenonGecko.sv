module XenonGecko(
		input logic clk_sys,		//ri and memory clock
		input logic clk_25,		//VGA clock
		input logic clk_250,		//HDMI clock
		input logic rst,
		// CPU interface
		//input logic ri_en,
		//input logic ri_wren,
		//input logic ri_ren,
		//input logic[3:0] ri_addr,
		//input logic[15:0] from_cpu,
		//output logic[15:0] to_cpu,
		// memory interface
		output logic mem_req,
		output logic mem_wren,
		input logic mem_ready,
		input logic[1:0] mem_offset,
		output logic[23:0] mem_addr,
		output logic[15:0] to_mem,
		input logic[15:0] from_mem,
		// HDMI interface
		output logic tmds_r_p,
		output logic tmds_r_n,
		output logic tmds_g_p,
		output logic tmds_g_n,
		output logic tmds_b_p,
		output logic tmds_b_n,
		output logic tmds_clk_p,
		output logic tmds_clk_n);

	// VGA timing logic
	logic[9:0] vesa_col;			//0 to 799 horizontal position in physical display
	logic[9:0] vesa_line;		//0 to 524 vertical position in physical siplay
	logic active_rows;				//use only to generate active area
	logic active_render_area;		//use for rendering
	logic active_render_rows;		//use for rendering
	logic[7:0] area_render_delay;
	//logic[7:0] rows_render_delay;
	//logic active_draw_rows;
	logic active_draw_area;
	logic hsync, vsync;	//active low
	logic[7:0] vsync_render_delay;
	logic[7:0] hsync_render_delay;
	logic draw_vsync;
	logic draw_hsync;
	logic vde;


	assign active_draw_area = area_render_delay[0];	//8 pixels behind render area
	//assign active_draw_rows = rows_render_delay[0];
	assign draw_vsync = vsync_render_delay[0];		//9 cycles behind render area
	assign draw_hsync = hsync_render_delay[0];

	always @(posedge clk_25)
	begin
		if(rst)
		begin
			vesa_col <= 10'h000;
			vesa_line <= 10'h000;
			active_rows <= 1'b1;
			active_render_area <= 1'b1;
			active_render_rows <= 1'b1;
		end
		else
		begin
			// line and pixel counters
			if(vesa_col == 10'd799)		//800 cycles per line
			begin
				vesa_col <= 10'h00;
				if(vesa_line == 10'd524)	//525 lines per frame
					vesa_line <= 10'h00;
				else
					vesa_line <= vesa_line + 10'h01;
			end
			else
				vesa_col <= vesa_col + 10'h01;
			// active area logic
			if(vesa_line == 10'd524)
				active_rows <= 1'b1;	//low at first cycle of 524, high at last cycle of 524
			if(vesa_line == 10'd479)
				active_rows <= 1'b0;	//low at last cycle of 479
			if(vesa_col == 10'd799)
				active_render_rows <= active_rows;
				
			if(active_rows && vesa_col == 10'd799)	//row is 524 or between 0 and 478 and col is 799
				active_render_area <= 1'b1;
			if(vesa_col == 10'd639)
				active_render_area <= 1'b0;	//active area is in rows 0 to 479, columns 0 to 639
		end
		area_render_delay <= {active_render_area, area_render_delay[7:1]};
		//rows_render_delay <= {active_render_rows, rows_render_delay[7:1]};
		vsync_render_delay <= {vsync, vsync_render_delay[7:1]};
		hsync_render_delay <= {hsync, hsync_render_delay[7:1]};
		vde <= active_draw_area;
		// HSYNC and VSYNC logic
		if(vesa_col < 10'd752 && vesa_col >= 10'd656)	//hsync starts at 656 and lasts 96 cycles
			hsync <= 1'b0;	//hsync is delayed by one cycle
		else
			hsync <= 1'b1;
		if((vesa_line == 10'd490) || (vesa_line == 10'd491))	//vsync starts at line 490 and lasts 2 lines
			vsync <= 1'b0;	//vsync is delayed by one cycle
		else
			vsync <= 1'b1;
	end

	//### Background rendering logic
	logic[4:0] bg_shift7;
	logic[4:0] bg_shift6;
	logic[4:0] bg_shift5;
	logic[4:0] bg_shift4;
	reg[7:0] bg_shift3;     //shift register for bit 3 of background palette index
	reg[7:0] bg_shift2;     //shift register for bit 2 of background palette index
	reg[7:0] bg_shift1;    //shift register for bit 1 of background palette index
	reg[7:0] bg_shift0;    //shift_register for bit 0 of background palette index

	logic[12:0] next_row_base;
	logic swap_pattern;
	logic swap_attribute;
	logic update_attribute;
	logic[1:0] next_line_pair;
	logic[7:0] pattern_address;
	logic[15:0] pattern_data;
	logic[3:0] attribute_data;

	assign next_line_pair = vesa_line[2:1] + 2'h1;
	assign pattern_address = vesa_col[9:2];
	assign swap_pattern = active_render_rows & (vesa_col == 10'd799) & vesa_line[0];
	assign swap_attribute = active_render_rows & (&vesa_line[2:0]) && (vesa_col == 10'd799);
	assign update_attribute = (vesa_col[2:0] == 3'h1);

	initial
	begin
		next_row_base = 13'h0000;
	end
	
	always @(posedge clk_25)
	begin
		if(active_render_rows && (vesa_col == 10'd799))
		begin
			if(vesa_line == 10'd479)
				next_row_base <= 13'h0000;
			else if(&vesa_line[2:0])
				next_row_base <= next_row_base + 13'd80;
		end

		if(active_render_area || active_draw_area)
		begin
			bg_shift0 <= {1'b0, bg_shift0[7:1]};
			bg_shift1 <= {1'b0, bg_shift1[7:1]};
			bg_shift2 <= {1'b0, bg_shift2[7:1]};
			bg_shift3 <= {1'b0, bg_shift3[7:1]};
			bg_shift4 <= {bg_shift4[4], bg_shift4[4:1]};
			bg_shift5 <= {bg_shift5[4], bg_shift5[4:1]};
			bg_shift6 <= {bg_shift6[4], bg_shift6[4:1]};
			bg_shift7 <= {bg_shift7[4], bg_shift7[4:1]};
		end

		if((&vesa_col[1:0]) && active_render_area)
		begin
			bg_shift0[7:4] <= {pattern_data[0], pattern_data[4], pattern_data[8], pattern_data[12]};
			bg_shift1[7:4] <= {pattern_data[1], pattern_data[5], pattern_data[9], pattern_data[13]};
			bg_shift2[7:4] <= {pattern_data[2], pattern_data[6], pattern_data[10], pattern_data[14]};
			bg_shift3[7:4] <= {pattern_data[3], pattern_data[7], pattern_data[11], pattern_data[15]};
			bg_shift4[4] <= attribute_data[0];
			bg_shift5[4] <= attribute_data[1];
			bg_shift6[4] <= attribute_data[2];
			bg_shift7[4] <= attribute_data[3];
		end
	end

	xgmm xgmm_inst(
		.clk_sys(clk_sys),
		.rst(rst),
		.swap_pattern(swap_pattern),
		.swap_attribute(swap_attribute),
		.update_attribute(update_attribute),
		.next_row_base(next_row_base),
		.next_line_pair(next_line_pair),
		.pattern_address(pattern_address),
		.odd_line(vesa_line[0]),
		.pattern_data(pattern_data),
		.attribute_data(attribute_data),
		.mem_req(mem_req),
		.mem_wren(mem_wren),
		.mem_ready(mem_ready),
		.mem_offset(mem_offset),
		.mem_addr(mem_addr),
		.to_mem(to_mem),
		.from_mem(from_mem));
		
	logic[7:0] bg_palette_index;
	logic[7:0] vga_r;
	logic[7:0] vga_g;
	logic[7:0] vga_b;
	assign bg_palette_index = {bg_shift7[0], bg_shift6[0], bg_shift5[0], bg_shift4[0], bg_shift3[0], bg_shift2[0], bg_shift1[0], bg_shift0[0]};
		
	XG_palette palette(.address(bg_palette_index), .clock(clk_25), .q({vga_b, vga_g, vga_r}));
	
	//generate a slightly delayed clk_25
//	logic clk_25_buf1, clk_25_buf2;
//	logic clk_25_delayed;
//	always @(negedge clk_sys)	//negedge of clk_sys is not alligned with clk_25 transitions
//	begin
//		clk_25_buf1 <= clk_25;	//clk_25_buf1 delayed by 10ns
//	end
//	always @(posedge clk_sys)
//	begin
//		clk_25_buf2 <= clk_25_buf1;	//clk_25_buf2 delayed by 20ns
//		clk_25_delayed <= clk_25_buf2;	//clk_25_delayed is delayed by 40ns (one clk_25 cycle) + register propagation delay
//	end
	
	vga_to_hdmi vga_to_hdmi_inst(
		.clk_25(clk_25),		// VGA clock
		//.clk_25_delayed(clk_25_delayed),
		.clk_250(clk_250),		// HDMI clock
		.vde(vde), 			// Active draw area
		// VGA interface
		.draw_vsync(draw_vsync),
		.draw_hsync(draw_hsync),
		.vga_r(vga_r),
		.vga_g(vga_g),
		.vga_b(vga_b),
		// HDMI interface
		.tmds_r_p(tmds_r_p),
		.tmds_r_n(tmds_r_n),
		.tmds_g_p(tmds_g_p),
		.tmds_g_n(tmds_g_n),
		.tmds_b_p(tmds_b_p),
		.tmds_b_n(tmds_b_n),
		.tmds_clk_p(tmds_clk_p),
		.tmds_clk_n(tmds_clk_n));

endmodule

module xgmm(
		input logic clk_sys,
		input logic rst,
		// XG interface
		input logic swap_pattern,
		input logic swap_attribute,
		input logic update_attribute,
		input logic[12:0] next_row_base,	//base address for the tile attributes in main memory
		input logic[1:0] next_line_pair,
		input logic[7:0] pattern_address,
		input logic odd_line,
		output logic[15:0] pattern_data,
		output logic[3:0] attribute_data,
		// memory interface
		output logic mem_req,
		output logic mem_wren,
		input logic mem_ready,
		input logic[1:0] mem_offset,
		output logic[23:0] mem_addr,
		output logic[15:0] to_mem,
		input logic[15:0] from_mem);
		
	assign to_mem = 16'h0000;
	enum logic[3:0] {
	S_IDLE,
	S_UPDATE_PATTERN_BEGIN,
	S_UPDATE_PATTERN_NOP,
	S_UPDATE_PATTERN_FETCH,
	S_UPDATE_PATTERN_INDEX,
	S_UPDATE_PATTERN_WRITE,
	S_UPDATE_ATTRIBUTE_FETCH,
	S_UPDATE_ATTRIBUTE_WRITE} state;

	logic active_pattern;	//selects the active pattern buffer
	logic active_attribute;
	logic update_attribute_flag;
	logic update_pattern_flag;
	logic[9:0] xgr_address_a;
	logic[9:0] xgr_address_b;
	logic xgr_wren_a;
	logic[15:0] from_xgr_a;
	logic[15:0] to_xgr_a;
	logic[15:0] from_xgr_b;
	logic prev_swap_pattern;
	logic prev_swap_attribute;
	logic prev_update_attribute;
	logic[3:0] attribute_data_hold;
	
	logic[6:0] buff_attribute_index;	//index into attribute buffer
	logic[6:0] prev_buff_attribute_index;	//used to generate pattern buffer address
	logic[4:0] attribute_offset;	//offset of tile attribute in main memory
	logic[12:0] mem_attribute_index;	//address of attribute entry in main memory
	logic[11:0] pattern_index;	//index into main memory pattern table

	assign mem_attribute_index = next_row_base + {attribute_offset, 2'b00};

	assign to_xgr_a = from_mem;

	assign xgr_address_b = update_attribute ? {active_attribute, 2'b11, pattern_address[7:1]} : {active_pattern, pattern_address[7:1], odd_line, pattern_address[0]};
	assign pattern_data = from_xgr_b;
	assign attribute_data = attribute_data_hold;
	
	initial
	begin
		active_pattern = 1'b0;
		active_attribute = 1'b0;
		attribute_data_hold = 4'h0;
		buff_attribute_index = 7'h00;
		attribute_offset = 5'h00;
		pattern_index = 12'h000;
	end
	
	always @(posedge clk_sys)
	begin
		prev_update_attribute <= update_attribute;
		if(rst)
		begin
			state <= S_IDLE;
			update_attribute_flag <= 1'b0;
			update_pattern_flag <= 1'b0;
			prev_swap_attribute <= 1'b0;
			prev_swap_pattern <= 1'b0;
			mem_addr <= 24'h000000;
			mem_req <= 1'b0;
		end
		else
		begin
			prev_swap_pattern <= swap_pattern;
			prev_swap_attribute <= swap_attribute;
			if(swap_pattern & ~prev_swap_pattern)
				active_pattern <= ~active_pattern;
			if(swap_attribute & ~prev_swap_attribute)
				active_attribute <= ~active_attribute;
			if(prev_update_attribute)
				attribute_data_hold <= from_xgr_b[15:12];
			case(state)
				S_IDLE:
				begin
					if(swap_pattern || update_pattern_flag || swap_attribute)
						state <= S_UPDATE_PATTERN_BEGIN;
					if(swap_attribute)
					begin
						attribute_offset <= 5'h00;
						update_attribute_flag <= 1'b1;
					end
					if(update_attribute_flag)
						state <= S_UPDATE_ATTRIBUTE_FETCH;
					buff_attribute_index <= 7'h00;
					prev_buff_attribute_index <= buff_attribute_index;
					mem_wren <= 1'b0;
				end
				S_UPDATE_PATTERN_BEGIN:
				begin
					buff_attribute_index <= buff_attribute_index + 7'h01;
					prev_buff_attribute_index <= buff_attribute_index;
					state <= S_UPDATE_PATTERN_NOP;
				end
				S_UPDATE_PATTERN_NOP:
				begin
					pattern_index <= from_xgr_a[11:0];
					state <= S_UPDATE_PATTERN_FETCH;
				end
				S_UPDATE_PATTERN_FETCH:
				begin
					mem_addr <= {8'h00, pattern_index, next_line_pair[1:0], 2'b00};
					mem_req <= 1'b1;
					mem_wren <= 1'b0;
					state <= S_UPDATE_PATTERN_INDEX;
				end
				S_UPDATE_PATTERN_INDEX:
				begin
					pattern_index <= from_xgr_a[11:0];
					state <= S_UPDATE_PATTERN_WRITE;
				end
				S_UPDATE_PATTERN_WRITE:
				begin
					mem_req <= 1'b0;
					if(mem_ready & (&mem_offset))
					begin
						buff_attribute_index <= buff_attribute_index + 7'h01;
						prev_buff_attribute_index <= buff_attribute_index;
						if(prev_buff_attribute_index == 7'd79)
						begin
							state <= S_IDLE;
							update_pattern_flag <= 1'b0;
						end
						else
							state <= S_UPDATE_PATTERN_FETCH;
					end
				end
				S_UPDATE_ATTRIBUTE_FETCH:
				begin
					mem_addr <= {8'h01, 3'b000, mem_attribute_index};
					mem_wren <= 1'b0;
					if(swap_pattern || update_pattern_flag)
					begin
						mem_req <= 1'b0;
						state <= S_UPDATE_PATTERN_BEGIN;
					end
					else
					begin
						mem_req <= 1'b1;
						state <= S_UPDATE_ATTRIBUTE_WRITE;
					end
				end
				S_UPDATE_ATTRIBUTE_WRITE:
				begin
					if(swap_pattern)
						update_pattern_flag <= 1'b1;
					mem_req <= 1'b0;
					if(mem_ready & (&mem_offset))
					begin
						attribute_offset <= attribute_offset + 5'h01;
						if(attribute_offset == 5'd19)
						begin
							state <= S_IDLE;
							update_attribute_flag <= 1'b0;
						end
						else
							state <= S_UPDATE_ATTRIBUTE_FETCH;
					end
				end
			endcase // state
		end
	end
	
	always_comb
	begin
		case(state)
			S_IDLE:
			begin
				xgr_address_a = 10'hxxx;
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_BEGIN:
			begin
				xgr_address_a = {active_attribute, 2'b11, buff_attribute_index};
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_NOP:
			begin
				xgr_address_a = 10'hxxx;
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_FETCH:
			begin
				xgr_address_a = {active_attribute, 2'b11, buff_attribute_index};
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_INDEX:
			begin
				xgr_address_a = 10'hxxx;
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_WRITE:
			begin
				xgr_address_a = {~active_pattern, prev_buff_attribute_index, mem_offset};
				xgr_wren_a = mem_ready;
			end
			S_UPDATE_ATTRIBUTE_FETCH:
			begin
				xgr_address_a = 10'hxxx;
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_ATTRIBUTE_WRITE:
			begin
				xgr_address_a = {~active_attribute, 2'b11, attribute_offset, mem_offset};
				xgr_wren_a = mem_ready;
			end
		endcase
	end
	
	XG_ram XG_ram_inst(
			.address_a(xgr_address_a),
			.address_b(xgr_address_b),
			.clock(clk_sys),
			.data_a(to_xgr_a),
			.data_b(16'h0000),
			.wren_a(xgr_wren_a),
			.wren_b(1'b0),
			.q_a(from_xgr_a),
			.q_b(from_xgr_b));

endmodule

module vga_to_hdmi(
		input logic clk_25,		// VGA clock
		//input logic clk_25_delayed,
		input logic clk_250,		// HDMI clock
		input logic vde, 			// Active draw area
		// VGA interface
		input logic draw_vsync,
		input logic draw_hsync,
		input logic[7:0] vga_r,
		input logic[7:0] vga_g,
		input logic[7:0] vga_b,
		// HDMI interface
		output logic tmds_r_p,
		output logic tmds_r_n,
		output logic tmds_g_p,
		output logic tmds_g_n,
		output logic tmds_b_p,
		output logic tmds_b_n,
		output logic tmds_clk_p,
		output logic tmds_clk_n);
		
	logic [9:0] TMDS_red, TMDS_green, TMDS_blue;
	logic [3:0] TMDS_mod10;  // modulus 10 counter
	logic [9:0] TMDS_shift_red, TMDS_shift_green, TMDS_shift_blue;
	logic TMDS_shift_load;
	logic TMDS_clk;

	always @(posedge clk_250)
	begin
		TMDS_shift_red   <= TMDS_shift_load ? TMDS_red   : {1'b0, TMDS_shift_red  [9:1]};
		TMDS_shift_green <= TMDS_shift_load ? TMDS_green : {1'b0, TMDS_shift_green[9:1]};
		TMDS_shift_blue  <= TMDS_shift_load ? TMDS_blue  : {1'b0, TMDS_shift_blue [9:1]};	
		TMDS_mod10 <= (TMDS_mod10 == 4'd9) ? 4'd0 : TMDS_mod10 + 4'd1;
		TMDS_shift_load <= (TMDS_mod10 == 4'd9);
		if((TMDS_mod10 == 4'd4) || (TMDS_mod10 == 4'd9))
		begin
			TMDS_clk <= (TMDS_mod10 == 4'd9);
		end
		
		tmds_r_p <= TMDS_shift_red[0];
		tmds_g_p <= TMDS_shift_green[0];
		tmds_b_p <= TMDS_shift_blue[0];
		tmds_clk_p <= TMDS_clk;
		tmds_r_n <= ~TMDS_shift_red[0];
		tmds_g_n <= ~TMDS_shift_green[0];
		tmds_b_n <= ~TMDS_shift_blue[0];
		tmds_clk_n <= ~TMDS_clk;
	end

	TMDS_encoder encode_R(.clk(clk_25), .VD(vga_r), .CD(2'b00), .VDE(vde), .TMDS(TMDS_red));
	TMDS_encoder encode_G(.clk(clk_25), .VD(vga_g), .CD(2'b00), .VDE(vde), .TMDS(TMDS_green));
	TMDS_encoder encode_B(.clk(clk_25), .VD(vga_b), .CD({~draw_vsync, ~draw_hsync}), .VDE(vde), .TMDS(TMDS_blue)); 

endmodule
