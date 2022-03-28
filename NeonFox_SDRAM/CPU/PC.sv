module PC(
		input logic clk,
		input logic rst,
		input logic pc_jmp,
		input logic pc_brx,
		input logic pc_brxt,
		input logic pc_call,
		input logic pc_ret,
		input logic H_en,
		input logic L_en,
		input logic interrupt,
		input logic[3:0] int_addr,
		input logic hazard,
		input logic data_hazard,
		input logic branch_hazard,
		input logic p_cache_miss,
		input logic[31:0] next_callx_addr,
		output logic[31:0] last_callx_addr,
		input logic[9:0] I_field,
		input logic n, z, p,
		output logic take_brx,
		output logic PC_stall,
		output logic[31:0] prg_address);
// PC sources:
	//PC + 1
	//brx
	//jmp, ret
	//int, A_miss_next
reg decoder_prev_hazard;
reg p_miss_override;
reg p_miss;
reg prev_p_miss;
logic prev_branch_taken;
logic prev_data_hazard;
reg[31:0] A_miss, A_miss_next;
reg[31:0] A_next_I;
reg[31:0] PC_reg;
reg[31:0] A_current_I, A_current_I_alternate, A_pipe0;
wire[31:0] stack_out;
wire[31:0] stack_in;
logic stack_en;
logic stack_push;
logic stack_pop;

assign take_brx = pc_brx & (pc_brxt ^ |({p, z, n, 1'b0} & (1 << {H_en, L_en}))) & ~branch_hazard;
assign stack_in = interrupt ? A_pipe0 : A_next_I;
assign stack_en = ~prev_data_hazard | interrupt;
assign stack_push = (pc_call & ~branch_hazard) | interrupt;
assign stack_pop = pc_ret & ~interrupt;

call_stack cstack0(.rst(rst), .clk(clk), .en(stack_en), .push(stack_push), .pop(stack_pop), .data_in(stack_in), .data_out(stack_out));

always @(posedge clk)
begin
	if(~hazard)
		A_next_I <= PC_reg;
	prev_data_hazard <= data_hazard;
	prev_branch_taken <= pc_jmp | (take_brx) | pc_call | pc_ret;
	PC_stall <= (prev_branch_taken & p_miss) | (PC_stall & p_miss);	// ???
	if(pc_jmp & ~interrupt)
		last_callx_addr <= stack_in;
end

always @(posedge clk or posedge rst)	//reset needs not be asynchronous, but doing this eliminates a mux and improves timing.
begin
	if(rst)
	begin
		decoder_prev_hazard <= 1'b0;
		p_miss_override <= 1'b0;
		p_miss <= 1'b0;
		prev_p_miss <= 1'b0;
		A_miss <= 32'h0000;
		A_miss_next <= 32'h0000;
		A_current_I_alternate <= 32'h0000;
		A_current_I <= 32'h0000;
		A_pipe0 <= 32'h0000;
	end
	else
	begin
		p_miss_override <= (hazard & ~p_miss) | (p_miss_override & hazard);	//set if hazard before p_miss, cleared when no hazard
		p_miss <= p_cache_miss;
		prev_p_miss <= p_miss;
		if(~p_miss | prev_branch_taken)
		begin
			//if(~p_miss)
			//	A_miss <= A_miss_next;
			//else
			//	A_miss <= PC_reg;
			A_miss_next <= PC_reg;
		end
		if(~p_miss)
		begin
			A_miss <= A_miss_next;
		end

		if(~p_cache_miss)
		begin
			decoder_prev_hazard <= hazard;
		end
		if(hazard && ~decoder_prev_hazard)
			A_current_I_alternate <= A_next_I;
		if(~hazard)
		begin
			if(decoder_prev_hazard)
				A_current_I <= A_current_I_alternate;
			else if(~(p_miss | prev_p_miss))
				A_current_I <= A_next_I;
			A_pipe0 <= A_current_I;
		end
		
		if(prev_branch_taken)	//assign new address to flushed instructions in case of interrupt.
		begin
			A_current_I_alternate <= PC_reg;
			A_current_I <= PC_reg;
			A_pipe0 <= PC_reg;	//TODO: does this change for delayed branches?
		end
	end
end

//mux priority:
// 0: interrupt
// 1: ((pc_jmp | pc_call) & ~branch_hazard)
// 2: pc_ret
// 3: take_brx
// 4: (~hazard & ~(p_miss & ~p_miss_override))
// 5: ((p_miss & ~p_miss_override) & ~prev_p_miss & ~prev_branch_taken)
//Note that 4 and 5 above are mutually exclusive.
// Alternative mux priority:
// 0: interrupt
// 1: ((pc_jmp | pc_call) & ~branch_hazard)
// 2: pc_ret
// 3: take_brx
// 4: (~hazard & ~(p_miss & ~p_miss_override))
// 5: (p_miss & ~prev_p_miss & ~prev_branch_taken)
//Note that mutual exclusivity between 4 and 5 is sacrificed to obtain a simpler expression for 5
always @(posedge clk or posedge rst)
begin
	if(rst)
	begin
		PC_reg <= 16'h0000;
	end
	else if(interrupt | (p_miss & ~prev_p_miss & ~prev_branch_taken) | ((pc_jmp | pc_call) & ~branch_hazard) | pc_ret | take_brx | (~hazard & ~(p_miss & ~p_miss_override)))
	begin
		if(interrupt | ((pc_jmp | pc_call) & ~branch_hazard) | pc_ret | ~(take_brx | (~hazard & ~(p_miss & ~p_miss_override))))
		begin
			if(interrupt | ~(((pc_jmp | pc_call) & ~branch_hazard) | pc_ret))
			begin
				if(interrupt)
				begin
					PC_reg <= {27'h0000000, int_addr, 1'b0};
				end
				else	//p_miss & ~prev_p_miss & ~prev_branch_taken
				begin
					PC_reg <= A_miss_next;
				end
			end
			else	//((pc_jmp | pc_call) & ~branch_hazard) | pc_ret
			begin
				if((pc_jmp | pc_call) & ~branch_hazard)
				begin
					PC_reg <= next_callx_addr;
				end
				else	//pc_ret
				begin
					PC_reg <= stack_out;
				end
			end
		end
		else	//(take_brx) | (~hazard & ~(p_miss & ~p_miss_override))
		begin
			if(take_brx)
			begin
				PC_reg <= A_pipe0 + {{22{I_field[9]}}, I_field};
			end
			else	//(~hazard & ~(p_miss & ~p_miss_override))
			begin
				PC_reg <= PC_reg + 32'h01;
			end
		end
	end
end

assign prg_address = p_miss ? A_miss : PC_reg;
endmodule : PC

module call_stack(input wire rst, clk, en, push, pop, input wire[31:0] data_in, output wire[31:0] data_out);
reg[3:0] address;
reg[31:0] input_buf;
reg[31:0] output_buf;
reg prev_push;
(* ramstyle = "logic" *) reg[31:0] stack_mem[15:0];

always @(posedge clk or posedge rst)
begin
	if(rst)
	begin
		address <= 4'b0000;
	end
	else if(en & (push | pop))
	begin
		address <= address + {{3{pop}}, 1'b1};
	end
end

always @(posedge clk)
begin
    if(en)
    begin
      input_buf <= data_in;
		prev_push <= push;
		output_buf <= stack_mem[address];
		if(prev_push)
			stack_mem[address] <= input_buf;
    end
end
assign data_out = output_buf;
endmodule
