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
