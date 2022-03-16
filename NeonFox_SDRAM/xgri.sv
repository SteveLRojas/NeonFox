module xgri(
		input logic clk_sys,
		input logic rst,
		// CPU interface
		input logic ri_en,
		input logic ri_wren,
		input logic ri_ren,
		input logic[3:0] ri_addr,
		input logic[15:0] from_cpu,
		output logic[15:0] to_cpu,
		// XGMM interface
		output logic p_full,
		output logic a_full,
		input logic p_pop,
		input logic a_pop,
		output logic[15:0] p_data,
		output logic[15:0] a_data,
		output logic[11:0] par,
		output logic[12:0] aar);

	//logic[11:0] par;
	//logic[15:0] pdr;
	//logic[12:0] aar;
	//logic[15:0] adr;
	
	logic p_empty;
	logic p_push;
	logic a_empty;
	logic a_push;
	logic prev_p_empty;
	logic prev_a_empty;
	
	assign p_push = ri_en & ri_wren & (ri_addr == 4'h3);
	assign a_push = ri_en & ri_wren & (ri_addr == 4'h5);
	
	always_ff @(posedge clk_sys or posedge rst)
	begin
		if(rst)
		begin
			par <= 12'h000;
			aar <= 13'h0000;
			to_cpu <= 16'h0000;
			prev_p_empty <= 1'b1;
			prev_a_empty <= 1'b1;
		end
		else
		begin
			prev_p_empty <= p_empty;
			prev_a_empty <= a_empty;
			if(p_empty & ~prev_p_empty)
			begin
				par <= par + 12'h001;
			end
			if(a_empty & ~prev_a_empty)
			begin
				aar <= aar + 13'h0001;
			end
		
			if(ri_en & ri_wren)
			begin
				case(ri_addr)
					4'h1: par  <= from_cpu;
					4'h2: aar  <= from_cpu;
					default: ;
				endcase
			end
			
			if(ri_en)
			begin
				case(ri_addr)
					4'h0: to_cpu <= {12'h000, p_full, p_empty, a_full, a_empty};
					4'h1: to_cpu <= par;
					4'h2: to_cpu <= aar;
					default: ;
				endcase
			end
		end
	end
	
	queue_16_16 p_fifo(.clk(clk_sys), .reset(rst), .push(p_push), .pop(p_pop), .full(p_full), .empty(p_empty), .din(from_cpu), .dout(p_data));
	queue_16_4  a_fifo(.clk(clk_sys), .reset(rst), .push(a_push), .pop(a_pop), .full(a_full), .empty(a_empty), .din(from_cpu), .dout(a_data));
	
endmodule
