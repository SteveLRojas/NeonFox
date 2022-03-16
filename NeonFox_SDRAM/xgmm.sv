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
	logic force_next_buff;	//hack to use the correct attribute buffer when rendering the first line pair of the next attribute row

	assign mem_attribute_index = next_row_base + {attribute_offset, 2'b00};
	assign force_next_buff = ~|next_line_pair;

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
				xgr_address_a = {active_attribute ^ force_next_buff, 2'b11, buff_attribute_index};
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_NOP:
			begin
				xgr_address_a = 10'hxxx;
				xgr_wren_a = 1'b0;
			end
			S_UPDATE_PATTERN_FETCH:
			begin
				xgr_address_a = {active_attribute ^ force_next_buff, 2'b11, buff_attribute_index};
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
