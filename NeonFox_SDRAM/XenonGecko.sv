module XenonGecko(
		input logic clk_sys,		//ri and memory clock
		input logic clk_25,		//VGA clock
		input logic clk_250,		//HDMI clock
		input logic rst,
		output logic hsync,
		output logic vsync,   //these are active low
		// CPU interface
		input logic ri_en,
		input logic ri_wren,
		input logic ri_ren,
		input logic[3:0] ri_addr,
		input logic[15:0] from_cpu,
		output logic[15:0] to_cpu,
		// memory interface
		output logic mem_req,
		output logic mem_wren,
		input logic mem_ready,
		input logic[1:0] mem_offset,
		output logic[16:0] mem_addr,
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
	//logic hsync, vsync;	//active low
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
	assign update_attribute = (vesa_col[2:0] == 3'h1);

	initial
	begin
		next_row_base = 13'd80;
	end
	
	always @(posedge clk_25)
	begin
		swap_pattern <= active_render_rows & (vesa_col == 10'd799) & vesa_line[0];
		swap_attribute <= active_render_rows & (&vesa_line[2:0]) && (vesa_col == 10'd799);
		if(active_render_rows && (vesa_col == 10'd799))
		begin
			if(vesa_line == 10'd471)
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

	logic[7:0] bg_palette_index;
	logic[7:0] vga_r;
	logic[7:0] vga_g;
	logic[7:0] vga_b;
	assign bg_palette_index = {bg_shift7[0], bg_shift6[0], bg_shift5[0], bg_shift4[0], bg_shift3[0], bg_shift2[0], bg_shift1[0], bg_shift0[0]};
		
	logic p_full;
	logic a_full;
	logic p_pop;
	logic a_pop;
	logic[15:0] p_data;
	logic[15:0] a_data;
	logic[11:0] par;
	logic[12:0] aar;
		
	xgri xgri_inst(
		.clk_sys(clk_sys),
		.rst(rst),
		// CPU interface
		.ri_en(ri_en),
		.ri_wren(ri_wren),
		.ri_ren(ri_ren),
		.ri_addr(ri_addr),
		.from_cpu(from_cpu),
		.to_cpu(to_cpu),
		// XGMM interface
		.p_full(p_full),
		.a_full(a_full),
		.p_pop(p_pop),
		.a_pop(a_pop),
		.p_data(p_data),
		.a_data(a_data),
		.par(par),
		.aar(aar));
	
	xgmm xgmm_inst(
		.clk_sys(clk_sys),
		.rst(rst),
		// XG interface
		.swap_pattern(swap_pattern),
		.swap_attribute(swap_attribute),
		.update_attribute(update_attribute),
		.next_row_base(next_row_base),
		.next_line_pair(next_line_pair),
		.pattern_address(pattern_address),
		.odd_line(vesa_line[0]),
		.pattern_data(pattern_data),
		.attribute_data(attribute_data),
		// XGRI interface
		.p_full(p_full),
		.a_full(a_full),
		.p_pop(p_pop),
		.a_pop(a_pop),
		.p_data(p_data),
		.a_data(a_data),
		.par(par),
		.aar(aar),
		// SDRAM interface
		.mem_req(mem_req),
		.mem_wren(mem_wren),
		.mem_ready(mem_ready),
		.mem_offset(mem_offset),
		.mem_addr(mem_addr),
		.to_mem(to_mem),
		.from_mem(from_mem));
		
		
	XG_palette palette(.address(bg_palette_index), .clock(clk_25), .q({vga_b, vga_g, vga_r}));
	
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
