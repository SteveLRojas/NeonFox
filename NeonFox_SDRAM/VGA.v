module VGA(
			input wire clk_sys,
			input wire clk_25,
			input wire clk_250,
			input wire rst,
			input wire VGA_MS,
			input wire VGA_WrEn,
			input wire[7:0] VGA_Din,
			input wire[11:0] VGA_A,
			output wire[7:0] VGA_Dout,
			output wire[2:0] TMDS,
			output wire TMDS_clk,
			output wire HSYNC, VSYNC);
        
reg VGA_mode;
wire[7:0] DD;
wire[11:0] DA;

always @(posedge clk_sys or posedge rst)
begin
    if(rst)
        VGA_mode <= 1'b0;
    else if(VGA_MS)
        VGA_mode <= VGA_Din[0];
end

VGA_RAM RAM_VGA(
		.clock(clk_sys),
		.wren_a(VGA_WrEn),
		.address_a(VGA_A),
		.data_a(VGA_Din),
		.q_a(VGA_Dout),
		.wren_b(1'b0),
		.address_b(DA),
		.data_b(8'hXX),
		.q_b(DD));
MC6847_gen4 VDG(clk_25, clk_250, rst, VGA_mode, DD[7], DD[6], DD, DA, TMDS, TMDS_clk, HSYNC, VSYNC);
endmodule
