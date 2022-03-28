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
		.IO_ren(IO_ren0),
		.data_ren(data_ren0),
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
		//.rst(decoder_rst | PC_stall),
		.rst(decoder_rst),
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
		.IO_ren(IO_ren0),
		.IO_wren1(IO_wren1), .IO_wren2(IO_wren2),
		.data_ren(data_ren0),
		.data_wren1(data_wren1), .data_wren2(data_wren2),
		.status_ren(status_ren),
		.d_cache_read_miss(d_cache_read_miss),
		.d_cache_write_miss(d_cache_write_miss),
		.hazard(hazard),
		.data_hazard(data_hazard),
		.branch_hazard(branch_hazard),
		.decoder_rst(decoder_rst));
endmodule : NeonFox