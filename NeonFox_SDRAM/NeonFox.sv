module NeonFox(
		input wire clk,
		input wire halt,
		input wire reset,
		input wire int_rq,	//interrupt request
		input wire[3:0] int_addr,	//interrupt address
		input wire p_cache_miss,	//program cache read miss
		input wire d_cache_read_miss,	//data cache read miss
		input wire d_cache_write_miss,	//data cache write miss
		input wire[15:0] prg_data,	//program data
		output wire[31:0] prg_address,	//program address
		output wire[31:0] data_address,	//NOT used for IO
		output wire[15:0] IO_address,
		output wire[15:0] data_out,	//data and IO out
		input wire[15:0] data_in,
		input wire[15:0] IO_in,
		output wire data_wren,	//data write enable
		output wire data_ren,	//data read enable
		output wire IO_wren,	//high during IO writes
		output wire IO_ren,		//IO read enable
		output wire H_en,	//high byte write enable for IO and data
		output wire L_en	//low byte write enable for IO and data
		);
logic prev_int_rq;
logic interrupt;
logic[15:0] DIO_in;

logic data_hazard;
logic regf_wren, regf_wren1, regf_wren2;
logic H_en0, H_en1, H_en2;
logic L_en0, L_en1, L_en2;
logic[4:0] src_raddr;
logic[4:0] dest_waddr, dest_waddr1, dest_waddr2;
logic[15:0] alu_out;
logic[15:0] a_data;
logic[15:0] b_data;
logic[31:0] last_callx_addr;
logic[31:0] next_callx_addr;

logic set_cc;
logic[9:0] I_field;
logic[7:0] I_field1;
logic[3:0] alu_op, alu_op1;
logic n, z, p;

logic pc_jmp, pc_jmp1;
logic pc_brx;
logic pc_brxt;
logic pc_call, pc_call1;
logic pc_ret, pc_ret1;
logic hazard;
logic branch_hazard;
logic take_brx, take_brx1;
logic PC_stall;

logic decoder_rst;
logic data_wren0, data_wren1, data_wren2;
logic data_ren0;
logic IO_wren0, IO_wren1, IO_wren2;
logic IO_ren0;
logic status_ren;
logic address_select, address_select1, address_select2;
logic data_select, data_select1, data_select2;
logic IO_select, IO_select1, IO_select2;

assign data_wren = data_wren2;
assign data_ren = data_ren0 & ~(data_select1 | data_select2);
assign IO_wren = IO_wren2;
assign IO_ren = IO_ren0 & ~(IO_select1 | IO_select2);
assign data_out = alu_out;
assign H_en = H_en2 | ~L_en2;	//both H_en and L_en low from the decoder indicate high and low bytes are swapped (both enabled).
assign L_en = L_en2 | ~H_en2;

always @(posedge clk)
begin
	DIO_in <= ({16{data_ren}} & data_in) | ({16{IO_ren}} & IO_in);
	
	if(reset)
	begin
		interrupt <= 1'b0;
		prev_int_rq <= 1'b1;
	end
	else
	begin
		prev_int_rq <= int_rq & (~decoder_rst | prev_int_rq);	//set when int_rq & ~decoder_rst, clear when ~int_rq
		interrupt <= int_rq & ~prev_int_rq & ~decoder_rst;
	end
end

always_ff @(posedge clk or posedge reset)
begin
	if(reset)
	begin
		regf_wren1 <= 1'b0;
		regf_wren2 <= 1'b0;
		alu_op1 <= 4'b0111;
		pc_jmp1 <= 1'b0;
		pc_call1 <= 1'b0;
		pc_ret1 <= 1'b0;
		take_brx1 <= 1'b0;
		data_wren1 <= 1'b0;
		data_wren2 <= 1'b0;
		IO_wren1 <= 1'b0;
		IO_wren2 <= 1'b0;
		address_select1 <= 1'b0;
		address_select2 <= 1'b0;
		data_select1 <= 1'b0;
		data_select2 <= 1'b0;
		IO_select1 <= 1'b0;
		IO_select2 <= 1'b0;
	end
	else if(~data_hazard)
	begin
		//take_brx1 <= take_brx;
		if(hazard)
		begin
			regf_wren1 <= 1'b0;
			alu_op1 <= 4'b0111;
			pc_jmp1 <= 1'b0;
			pc_call1 <= 1'b0;
			pc_ret1 <= 1'b0;
			take_brx1 <= 1'b0;
			data_wren1 <= 1'b0;
			IO_wren1 <= 1'b0;
			address_select1 <= 1'b0;
			data_select1 <= 1'b0;
			IO_select1 <= 1'b0;
		end
		else	//not hazard
		begin
			regf_wren1 <= regf_wren;
			alu_op1 <= alu_op;
			pc_jmp1 <= pc_jmp;
			pc_call1 <= pc_call;
			pc_ret1 <= pc_ret;
			take_brx1 <= take_brx;
			data_wren1 <= data_wren0;
			IO_wren1 <= IO_wren0;
			address_select1 <= address_select;
			data_select1 <= data_select;
			IO_select1 <= IO_select;
		end
		regf_wren2 <= regf_wren1;
		data_wren2 <= data_wren1;
		IO_wren2 <= IO_wren1;
		address_select2 <= address_select1;
		data_select2 <= data_select1;
		IO_select2 <= IO_select1;
	end
end

always_ff @(posedge clk)
begin
	if(~data_hazard)
	begin
		set_cc <= (dest_waddr == 5'h1E) & regf_wren;
		H_en1 <= H_en0;
		H_en2 <= H_en1;
		L_en1 <= L_en0;
		L_en2 <= L_en1;
		dest_waddr1 <= dest_waddr;
		dest_waddr2 <= dest_waddr1;
		I_field1 <= I_field[7:0];
	end
end

//##### REGISTER FILE #####
reg_file reg_file_inst(
		.clk(clk),
		.data_hazard(data_hazard),
		.wren(regf_wren2),
		.h_en(H_en2),
		.l_en(L_en2),
		.data_select(data_select2),
		.IO_select(IO_select2),
		.a_address(src_raddr),
		.w_address(dest_waddr2),
		.w_data(alu_out),
		.a_data(a_data),
		.b_data(b_data),
		.IO_ren(IO_ren),
		.data_ren(data_ren),
		.DIO_in(DIO_in),
		.last_callx_addr(last_callx_addr),
		.next_callx_addr(next_callx_addr),
		.data_address(data_address),
		.IO_address(IO_address));

//##### ALU #####
ALU ALU_inst(
		.clk(clk),
		.data_hazard(data_hazard),
		.set_cc(set_cc), 
		.h_en(H_en1), .l_en(L_en1),
		.in_a(a_data), 
		.in_b(b_data),
		.I_field(I_field1),
		.alu_op(alu_op1),
		.alu_out(alu_out),
		.n(n), .z(z), .p(p));

//##### PC #####
PC PC_inst(
		.clk(clk),
		.rst(reset),
		.pc_jmp(pc_jmp),
		.pc_brx(pc_brx),
		.pc_brxt(pc_brxt),
		.pc_call(pc_call),
		.pc_ret(pc_ret),
		.H_en(H_en0),
		.L_en(L_en0),
		.interrupt(interrupt),
		.int_addr(int_addr),
		.hazard(hazard),
		.data_hazard(data_hazard),
		.branch_hazard(branch_hazard),
		.p_cache_miss(p_cache_miss),
		.next_callx_addr(next_callx_addr),
		.last_callx_addr(last_callx_addr),
		.I_field(I_field),
		.n(n), .z(z), .p(p),
		.take_brx(take_brx),
		.PC_stall(PC_stall),
		.prg_address(prg_address));

//##### DECODER #####
decode_unit decoder_inst(
		.clk(clk),
		.rst(decoder_rst | PC_stall),
		.hazard(hazard),
		.p_cache_miss(p_cache_miss),
		.prg_data(prg_data),
		.data_wren(data_wren0),
		.data_ren(data_ren0),
		.IO_wren(IO_wren0),
		.IO_ren(IO_ren0),
		.status_ren(status_ren),
		.address_select(address_select),
		.data_select(data_select),
		.IO_select(IO_select),
		.H_en(H_en0),
		.L_en(L_en0),
		.alu_op(alu_op),
		.I_field(I_field),
		.src_raddr(src_raddr),
		.dest_waddr(dest_waddr),
		.regf_wren(regf_wren),
		.pc_jmp(pc_jmp),
		.pc_brx(pc_brx),
		.pc_brxt(pc_brxt),
		.pc_call(pc_call),
		.pc_ret(pc_ret));

//##### HAZARD UNIT #####
hazard_unit hazard_inst(
		.clk(clk),
		.pc_jmp(pc_jmp), .pc_jmp1(pc_jmp1),
		.pc_brx(pc_brx),
		.pc_call(pc_call), .pc_call1(pc_call1),
		.pc_ret1(pc_ret1),
		.interrupt(interrupt),
		.take_brx1(take_brx1),
		.alu_op1(alu_op1),
		.halt(halt),
		.rst(reset),
		.IO_select1(IO_select1), .IO_select2(IO_select2),
		.address_select1(address_select1), .address_select2(address_select2),
		.data_select1(data_select1), .data_select2(data_select2), 
		.IO_ren(IO_ren),
		.IO_wren1(IO_wren1), .IO_wren2(IO_wren2),
		.data_ren(data_ren),
		.data_wren1(data_wren1), .data_wren2(data_wren2),
		.status_ren(status_ren),
		.d_cache_read_miss(d_cache_read_miss),
		.d_cache_write_miss(d_cache_write_miss),
		.hazard(hazard),
		.data_hazard(data_hazard),
		.branch_hazard(branch_hazard),
		.decoder_rst(decoder_rst));
endmodule : NeonFox

module ALU(
		input logic clk,
		input logic data_hazard,
		input logic set_cc, 
		input logic h_en, l_en,
		input logic[15:0] in_a, 
		input logic[15:0] in_b,
		input logic[7:0] I_field,
		input logic[3:0] alu_op,
		output logic[15:0] alu_out,
		output logic n, z, p);
logic[16:0] add_result;
logic[15:0] alu_result;
logic c;
logic swap;
logic[15:0] bit_sel;
logic[7:0] immediate_high;
logic[7:0] immediate_low;

assign bit_sel = 16'h01 << I_field[3:0];
assign swap = ~h_en & ~l_en;
assign add_result = in_a + (in_b ^ {16{alu_op[0]}}) + (alu_op[3] ? c : alu_op[0]);
assign immediate_low = I_field;
assign immediate_high = ~l_en ? I_field : {8{I_field[7]}};	//if swap or high duplicate instead of sign extend

always_comb
begin
	case(alu_op[2:0])
		3'b000: alu_result = add_result[15:0];	//add, addc
		3'b001: alu_result = add_result[15:0];	//sub, subc
		3'b010: alu_result = in_a ^ {16{alu_op[3]}};	//move, not
		3'b011: alu_result = alu_op[3] ? {in_a[14:0], in_a[15]} : {in_a[0], in_a[15:1]};	//ror, rol
		3'b100: alu_result = in_a & in_b;	//and
		3'b101: alu_result = in_a ^ in_b;	//xor
		3'b110: alu_result = in_a | in_b;	//or
		3'b111: alu_result = alu_op[3] ? in_a : {immediate_high, immediate_low};	//alu nop, bitt
	endcase // alu_op[2:0]
end
always_ff @(posedge clk)
begin
	if(~data_hazard)
	begin
		alu_out <= swap ? {alu_result[7:0], alu_result[15:8]} : alu_result;	//no need to mask bytes not enabled, the register file will ignore them.
	end
	if(~data_hazard & (alu_op[3] | ~(&alu_op[2:0]) | set_cc))	// not hazard and not alu nop or set_cc
	begin
		if(&alu_op[2:0])	//bitt
		begin
			n <= 1'b0;
			z <= ~(|(in_a & bit_sel));	//hardware for bitt need not care about h_en and l_en, this can be handled by the assembler.
			p <= 1'b0;
		end
		else if(~set_cc)	//normal alu ops
		begin
			n <= h_en ? alu_result[15] : alu_result[7];
			z <= ~( ((|alu_result[15:8]) & (h_en | swap)) | ((|alu_result[7:0]) & (l_en | swap)) );
			p <= ( ((|alu_result[15:8]) & (h_en | swap)) | ((|alu_result[7:0]) & (l_en | swap)) ) & ~(h_en ? alu_result[15] : alu_result[7]);
		end
		else	//set_cc
		begin
			n <= in_a[2];
			z <= in_a[1];
			p <= in_a[0];
		end
	end
	if(~data_hazard & (~(|alu_op[2:1]) | set_cc))	//not hazard and (add | addc | sub | subc) or set_cc
	begin
		if(set_cc)
		begin
			c <= in_a[3];
		end
		else
		begin
			c <= (h_en ? add_result[16] : add_result[8]);
		end
	end
end
endmodule : ALU

module decode_unit(
		input logic clk,
		input logic rst,
		input logic hazard,
		input logic p_cache_miss,
		input logic[15:0] prg_data,
		output logic data_wren,
		output logic data_ren,
		output logic IO_wren,
		output logic IO_ren,
		output logic status_ren,
		output logic address_select,
		output logic data_select,
		output logic IO_select,
		output logic H_en,
		output logic L_en,
		output logic[3:0] alu_op,
		output logic[9:0] I_field,
		output logic[4:0] src_raddr,
		output logic[4:0] dest_waddr,
		output logic regf_wren,
		output logic pc_jmp,
		output logic pc_brx,
		output logic pc_brxt,
		output logic pc_call,
		output logic pc_ret);
localparam ALU_ADD = 4'b0000;
localparam ALU_ADDC = 4'b1000;
localparam ALU_SUB = 4'b0001;
localparam ALU_SUBC = 4'b1001;
localparam ALU_MOVE = 4'b0010;
localparam ALU_NOT = 4'b1010;
localparam ALU_ROR = 4'b0011;
localparam ALU_ROL = 4'b1011;
localparam ALU_AND = 4'b0100;
localparam ALU_XOR = 4'b0101;
localparam ALU_OR = 4'b0110;
//localparam ALU_LIM = 4'b1110;
localparam ALU_NOP = 4'b0111;
localparam ALU_BITT = 4'b1111;

reg[15:0] I_reg;
reg[15:0] I_alternate;
reg prev_hazard;
reg prev_p_cache_miss;

always_ff @(posedge clk)
begin
	if(rst)
	begin
		I_reg <= 16'hC000;
		I_alternate <= 16'hC000;
		prev_hazard <= 1'b0;
		prev_p_cache_miss <= 1'b0;
	end
	else 
	begin
		prev_p_cache_miss <= p_cache_miss;
		if(~p_cache_miss)
			prev_hazard <= hazard;
		if(hazard & (~prev_hazard))
			I_alternate <= prg_data;
		if(~hazard)
		begin
			if(prev_hazard)	//loads last valid instruction after hazard.
				I_reg <= I_alternate;
			else if(p_cache_miss | prev_p_cache_miss)
				I_reg <= 16'hC000;	//NOP
			else
				I_reg <= prg_data;
		end
	end
	
	if(rst)
	begin
		pc_jmp <= 1'b0;
		pc_brx <= 1'b0;
		pc_call <= 1'b0;
		pc_ret <= 1'b0;
		regf_wren <= 1'b0;
		data_wren <= 1'b0;
		data_ren <= 1'b0;
		IO_wren <= 1'b0;
		IO_ren <= 1'b0;
		status_ren <= 1'b0;
		alu_op <= 4'h0;
	end
	else if(~hazard)
	begin
		I_field <= I_reg[9:0];
		H_en <= I_reg[11];
		L_en <= I_reg[10];
		src_raddr <= I_reg[9:5];
		pc_brxt <= I_reg[12];
		case(I_reg[15:12])
			4'h0:	//add
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_ADD;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h1:	//addc
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_ADDC;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h2:	//sub
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_SUB;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h3:	//subc
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_SUBC;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h4:	//move, test
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_MOVE;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h5:	//not
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_NOT;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h6:	//ror
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_ROR;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h7:	//rol
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_ROL;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h8:	//and
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_AND;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'h9:	//xor
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_XOR;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'ha:	//or
			begin
				data_wren <= (I_reg[4:0] == 5'h18);
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= (I_reg[4:0] == 5'h19);
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= (I_reg[4:0] == 5'h1A) | (I_reg[4:0] == 5'h1B);
				data_select <= (I_reg[4:0] == 5'h14) | (I_reg[4:0] == 5'h15);
				IO_select <= (I_reg[4:0] == 5'h16);
				alu_op <= ALU_OR;
				dest_waddr <= I_reg[4:0];
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'hb:	//call, callx, calll, calllx, ret, retx, retl, retlx, jmp, jmpl
			begin
				data_wren <= 1'b0;
				data_ren <= 1'b0;
				IO_wren <= 1'b0;
				IO_ren <= 1'b0;
				status_ren <= 1'b0;
				address_select <= 1'b0;
				data_select <= 1'b0;
				IO_select <= 1'b0;
				alu_op <= ALU_NOP;
				dest_waddr <= 5'h11;	//AUX1
				regf_wren <= |I_reg[11:10];
				pc_jmp <= I_reg[8];
				pc_brx <= 1'b0;
				pc_call <= ~I_reg[8] & ~I_reg[9];
				pc_ret <= ~I_reg[8] & I_reg[9];
			end
			4'hc:	//nop, brn, brz, brp
			begin
				data_wren <= 1'b0;
				data_ren <= 1'b0;
				IO_wren <= 1'b0;
				IO_ren <= 1'b0;
				status_ren <= 1'b0;
				address_select <= 1'b0;
				data_select <= 1'b0;
				IO_select <= 1'b0;
				alu_op <= ALU_NOP;
				dest_waddr <= 5'hxx;
				regf_wren <= 1'b0;
				pc_jmp <= 1'b0;
				pc_brx <= |I_reg[11:10];	//no brx for nop
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'hd:	//bra, brnn, brnz, brnp
			begin
				data_wren <= 1'b0;
				data_ren <= 1'b0;
				IO_wren <= 1'b0;
				IO_ren <= 1'b0;
				status_ren <= 1'b0;
				address_select <= 1'b0;
				data_select <= 1'b0;
				IO_select <= 1'b0;
				alu_op <= ALU_NOP;
				dest_waddr <= 5'hxx;
				regf_wren <= 1'b0;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b1;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'he:	//lim
			begin
				data_wren <= 1'b0;
				data_ren <= 1'b0;
				IO_wren <= 1'b0;
				IO_ren <= 1'b0;
				status_ren <= 1'b0;
				address_select <= 1'b0;
				data_select <= 1'b0;
				IO_select <= 1'b0;
				alu_op <= ALU_NOP;
				dest_waddr <= {3'b100, I_reg[9:8]};
				regf_wren <= 1'b1;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
			4'hf:	//bitt
			begin
				data_wren <= 1'b0;
				data_ren <= (I_reg[9:5] == 5'h18);
				IO_wren <= 1'b0;
				IO_ren <= (I_reg[9:5] == 5'h19);
				status_ren <= (I_reg[9:5] == 5'h1E);
				address_select <= 1'b0;
				data_select <= 1'b0;
				IO_select <= 1'b0;
				alu_op <= ALU_BITT;
				dest_waddr <= 5'hxx;
				regf_wren <= 1'b0;
				pc_jmp <= 1'b0;
				pc_brx <= 1'b0;
				pc_call <= 1'b0;
				pc_ret <= 1'b0;
			end
		endcase
	end
end
endmodule : decode_unit

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
reg prev_hazard;
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
	PC_stall <= (prev_branch_taken & p_miss) | (PC_stall & p_miss);
	if(pc_jmp & ~interrupt)
		last_callx_addr <= stack_in;
end

always @(posedge clk or posedge rst)	//reset needs not be asynchronous, but doing this eliminates a mux and improves timing.
begin
	if(rst)
	begin
		prev_hazard <= 1'b0;
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
		p_miss <= p_cache_miss;
		prev_p_miss <= p_miss;
		if(~p_miss | prev_branch_taken)
		begin
			if(~p_miss)
				A_miss <= A_miss_next;
			else
				A_miss <= PC_reg;
			A_miss_next <= PC_reg;
		end

		if(~p_cache_miss)
		begin
			prev_hazard <= hazard;
		end
		if(hazard && ~prev_hazard)
			A_current_I_alternate <= A_next_I;
		if(~hazard)
		begin
			if(prev_hazard)
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

always @(posedge clk or posedge rst)
begin
	if(rst)
	begin
		PC_reg <= 16'h0000;
	end
	else if(interrupt | (p_miss & ~prev_p_miss & ~prev_branch_taken) | ((pc_jmp | pc_call) & ~branch_hazard) | pc_ret | take_brx | (~hazard & ~p_miss))
	begin
		if(interrupt | (p_miss & ~prev_p_miss & ~prev_branch_taken & ~take_brx) | ((pc_jmp | pc_call) & ~branch_hazard) | pc_ret)
		begin
			//if(interrupt | (p_miss & ~prev_p_miss))
			if(interrupt | ~(((pc_jmp | pc_call) & ~branch_hazard) | pc_ret))
			begin
				if(interrupt)
				begin
					PC_reg <= {27'h0000000, int_addr, 1'b0};
				end
				else	//p_miss & ~prev_p_miss
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
		else	//(take_brx) | (~hazard & ~p_miss)
		begin
			if(take_brx)
			begin
				PC_reg <= A_pipe0 + {{22{I_field[9]}}, I_field};
			end
			else	//(~hazard & ~p_miss)
			begin
				PC_reg <= PC_reg + 32'h01;
			end
		end
	end
end

assign prg_address = p_miss ? A_miss : PC_reg;
endmodule : PC

module hazard_unit(
		input logic clk,
		input logic pc_jmp, pc_jmp1,
		input logic pc_brx,
		input logic pc_call, pc_call1,
		input logic pc_ret1,
		input logic interrupt,
		input logic take_brx1,
		input logic[3:0] alu_op1,
		input logic halt,
		input logic rst,
		input logic IO_select1, IO_select2,
		input logic address_select1, address_select2,
		input logic data_select1, data_select2, 
		input logic IO_ren,
		input logic IO_wren1, IO_wren2,
		input logic data_ren,
		input logic data_wren1, data_wren2,
		input logic status_ren,
		input logic d_cache_read_miss,
		input logic d_cache_write_miss,
		output logic hazard,
		output logic data_hazard,
		output logic branch_hazard,
		output logic decoder_rst);
logic rst_hold;
logic branch_taken;
logic decoder_flush;
logic status_hazard;
logic IO_hazard, IO_hazard1, IO_hazard2;
logic data_hazard_read1, data_hazard_read2;
logic data_hazard_read;
//logic data_hazard_read_miss;
//logic data_hazard_write_miss;
logic branch_hazard_ca;
logic branch_hazard_nzp;

assign status_hazard = (alu_op1 != 4'b0111) & status_ren;
assign branch_hazard_nzp = (alu_op1 != 4'b0111) & pc_brx;	//recent write to nzp and brx
assign branch_hazard_ca = (address_select1 | address_select2) & (pc_call | pc_jmp);	//call or jump after writing call address
assign branch_hazard = branch_hazard_ca | branch_hazard_nzp;
assign decoder_flush = take_brx1 | pc_jmp1 | pc_call1 | pc_ret1 | interrupt;
always_ff @(posedge clk) rst_hold <= rst;
assign decoder_rst = decoder_flush | rst | rst_hold;
assign IO_hazard1 = IO_ren & (IO_select1 | IO_wren1);
assign IO_hazard2 = IO_ren & (IO_select2 | IO_wren2);
assign IO_hazard = IO_hazard1 | IO_hazard2;
assign data_hazard_read1 = data_ren & (data_select1 | data_wren1);
assign data_hazard_read2 = data_ren & (data_select2 | data_wren2);
assign data_hazard_read = data_hazard_read1 | data_hazard_read2;
//assign data_hazard_read_miss = IO_ren & d_cache_read_miss;
//assign data_hazard_write_miss = IO_wren2 & d_cache_write_miss;
assign hazard = IO_hazard | data_hazard_read | d_cache_read_miss | d_cache_write_miss | halt | branch_hazard | status_hazard;
assign data_hazard = d_cache_write_miss;
endmodule : hazard_unit

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
	else if(en)
	begin
		input_buf <= data_in;
		prev_push <= push;
		output_buf <= stack_mem[address];
		if(prev_push)
			stack_mem[address] <= input_buf;
		if(push | pop)
			address <= address + {{3{pop}}, 1'b1};
	end
end
assign data_out = output_buf;
endmodule
