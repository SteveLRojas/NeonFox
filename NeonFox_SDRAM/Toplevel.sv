//                     /\         /\__
//                   // \       (  0 )_____/\            __
//                  // \ \     (vv          o|          /^v\
//                //    \ \   (vvvv  ___-----^        /^^/\vv\
//              //  /     \ \ |vvvvv/               /^^/    \v\
//             //  /       (\\/vvvv/              /^^/       \v\
//            //  /  /  \ (  /vvvv/              /^^/---(     \v\
//           //  /  /    \( /vvvv/----(O        /^^/           \v\
//          //  /  /  \  (/vvvv/               /^^/             \v|
//        //  /  /    \( vvvv/                /^^/               ||
//       //  /  /    (  vvvv/                 |^^|              //
//      //  / /    (  |vvvv|                  /^^/            //
//     //  / /   (    \vvvvv\          )-----/^^/           //
//    // / / (          \vvvvv\            /^^^/          //
//   /// /(               \vvvvv\        /^^^^/          //
//  ///(              )-----\vvvvv\    /^^^^/-----(      \\
// //(                        \vvvvv\/^^^^/               \\
///(                            \vvvv^^^/                 //
//                                \vv^/         /        //
//                                             /<______//
//                                            <<<------/
//                                             \<
//                                              \
//***************************************************
//* Test platform for NeonFox processors.           *
//* Copyright (C) 2021 Esteban Looser-Rojas.        *
//* Instantiates the NeonFox CPU core along with    *
//* cache controllers, an SDRAM controller, display *
//* controller, and input/output pripherals.        *
//***************************************************
module NeonFox_PVP(
		input wire reset,
		input wire clk,
		input wire button,
		output wire[1:0] LED,
		
		input wire RXD,
		output wire TXD,
		
		input wire ps2_clk_d,
		input wire ps2_data_d,
		output wire ps2_clk_q,
		output wire ps2_data_q,
		
		output wire sdram_clk,
		output wire sdram_cke,
		output wire sdram_cs_n,
		output wire sdram_wre_n,
		output wire sdram_cas_n,
		output wire sdram_ras_n,
		output wire[12:0] sdram_a,
		output wire[1:0] sdram_ba,
		output wire[1:0] sdram_dqm,
		inout wire[15:0] sdram_dq,
		
      // HDMI interface
		output logic tmds_r_p,
		output logic tmds_r_n,
		output logic tmds_g_p,
		output logic tmds_g_n,
		output logic tmds_b_p,
		output logic tmds_b_n,
		output logic tmds_clk_p,
		output logic tmds_clk_n,
		
		output wire[2:0] seg_sel,
		output wire[7:0] hex_out
		);

//Address map for IO space:
// 0x0000 to 0x0FFF	unused
// 0x1000 to 0xFFDF	unused
// 0xFFE0 to 0xFFE7	XGRI (read write)
// 0xFFE8 to 0xFFED	unused
// 0xFFEE to 0xFFEF	interrupt controller (read write)
// 0xFFF0 to 0xFFF3	timer module (read write)
// 0xFFF4 to 0xFFF5	keyboard module (read write)
// 0xFFF6 to 0xFFF7	unused
// 0xFFF8 to 0xFFFB	memory subsystem control registers	(write only)
// 0xFFFC to 0xFFFD	hex display
// 0xFFFE to 0xFFFF	RS-232 module	(read write)

//Address map for data space:
// 0x00000000 to 0x00FFFFFF	32MB main memory (cached)
// 0x01000000 to 0xFFFFFFFF	Not implemented (cached)

//Address map for program space:
// 0x00000000 to 0x00FFFFFF	32MB main memory (cached)
// 0x01000000 to 0xFFFFFFFF	Not implemented (cached)

// MSC Address map
// 0x0 program space control register
// 	bit0	reset bit
// 	bit1	reserved
// 	bit2	reserved
// 	bit3	control enable bit
// 0x1 program space page register
// 0x2 data space control register
// 	bit0	reset bit
// 	bit1	flush bit
// 	bit2	reserved
// 	bit3	control enable bit
// 0x3 data space page register

// RS-232 module address map
// 0 data register
// 1 status register
//		bit 0: TX overwrite
//		bit 1: RX overwrite
//		bit 2: TX ready
//		bit 3: RX ready
//		bit 4: TX queue empty
//		bit 5: RX queue full

// Keyboard module memory map
// 0 data register
// 1 status register
//		bit 0: TX overwrite
//		bit 1: RX overwrite
//		bit 2: TX ready
//		bit 3: RX ready
//		bit 4: TX queue empty
//		bit 5: RX queue full

// Timer module address map
// 0 counter bits 7:0
// 1 counter bits 15:8
// 2 counter bits 23:16
// 3 status
//		bit 0: counter 7:0 not zero
//		bit 1: counter 15:8 not zero
//		bit 2: counter 23:16 not zero
//		bit 3: counter 23:0 not zero
//		bit 4: VSYNC
//		bit 5: HSYNC

//####### PLL #################################################################
wire clk_250;
wire clk_25;
wire clk_sys;
PLL0 PLL_inst(.inclk0(clk), .c0(clk_25), .c1(clk_sys), .c2(sdram_clk), .c3(clk_250));
//#############################################################################

//####### IO Control #########################################################
wire[15:0] IO_address;
wire[15:0] from_cpu;
wire[15:0] IO_to_cpu;
wire IO_wren;
wire IO_ren;
wire L_en;
wire H_en;
wire hex_en;

reg button_s;
logic rst_s;
reg rst;
reg[14:0] hex_indicators;

assign hex_en = (&IO_address[15:2]) & ~IO_address[1];

initial
begin
	rst_s = 1'b1;
	rst = 1'b1;
	button_s = 1'b0;
	hex_indicators = 15'h0000;
end

always @(posedge clk_25)
begin
	button_s <= ~button;
	rst_s <= ~reset;
	rst <= rst_s;
end

always @(posedge clk_sys)
begin
	if(IO_wren & H_en & hex_en)
		hex_indicators[14:8] <= from_cpu[14:8];
	if(IO_wren & L_en & hex_en)
		hex_indicators[7:0] <= from_cpu[7:0];
end

MULTIPLEXED_HEX_DRIVER_3D multi_hex(
			.Clk(clk_25),
			.SEG0(hex_indicators[3:0]),
			.SEG1(hex_indicators[7:4]),
			.SEG2(hex_indicators[11:8]),
			.LED(hex_indicators[14:12]),
			.SEG_SEL(seg_sel),
			.HEX_OUT(hex_out));
//#############################################################################

//####### Program Cache #######################################################
wire[31:0] prg_address;
wire[15:0] prg_data;
wire[31:0] p1_address;
wire p1_req;
wire p1_ready;
wire[1:0] p1_offset;
wire[15:0] from_mem;
wire p_cache_rst;
wire p_cache_flush;
wire p_cache_prefetch;
wire p_cache_miss;
//assign p_cache_prefetch = 1'b0;
assign p_cache_flush = 1'b0;

cache_8K_2S_16 p_cache_inst(
		.clk(clk_sys),
		.rst(p_cache_rst),
		.flush(p_cache_flush),
		.prefetch(p_cache_prefetch),
		.write_miss(),
		.read_miss(p_cache_miss),
		.CPU_wren(1'b0),
		.CPU_ren(1'b1),
		.CPU_address(prg_address),
		.to_CPU(prg_data),
		.from_CPU(16'h00),
		.mem_address(p1_address),
		.from_mem(from_mem),
		.to_mem(),
		.mem_offset(p1_offset),
		.mem_req(p1_req),
		.mem_wren(),
		.mem_ready(p1_ready));
//#############################################################################

//####### Data Cache ##########################################################
wire[31:0] data_address;
wire[15:0] data_to_cpu;
wire data_wren;
wire data_ren;
wire[31:0] p2_address;
wire p2_req;
wire p2_wren;
wire p2_ready;
wire[1:0] p2_offset;
wire[15:0] p2_to_mem;
wire d_cache_rst;
wire d_cache_flush;
wire d_cache_prefetch;
wire d_read_miss;
wire d_write_miss;
//assign d_cache_prefetch = 1'b0;
		
cache_8K_2S_16 d_cache_inst(
		.clk(clk_sys),
		.rst(d_cache_rst),
		.flush(d_cache_flush),
		.prefetch(d_cache_prefetch),
		.write_miss(d_write_miss),
		.read_miss(d_read_miss),
		.CPU_wren(data_wren),
		.CPU_ren(data_ren),
		.CPU_address(data_address),
		.to_CPU(data_to_cpu),
		.from_CPU(from_cpu),
		.mem_address(p2_address),
		.from_mem(from_mem),
		.to_mem(p2_to_mem),
		.mem_offset(p2_offset),
		.mem_req(p2_req),
		.mem_wren(p2_wren),
		.mem_ready(p2_ready));
//#############################################################################

//####### Main Memory #########################################################
wire init_req;
wire init_ready;
wire[15:0] init_data;
wire[11:0] init_address;

logic[16:0] p3_address;
logic[15:0] p3_to_mem;
logic p3_req;
logic p3_wren;
logic p3_ready;
logic[1:0] p3_offset;

SDRAM_TP16_I SDRAM_controller(
		.clk(clk_sys),
		.rst(rst),
		.from_mem(from_mem),
		
		.p1_address(p1_address[23:0]),		//	Program cache
		.p1_req(p1_req),
		.p1_ready(p1_ready),
		.p1_offset(p1_offset),
		
		.p2_address(p2_address[23:0]),		// Data cache
		.p2_to_mem(p2_to_mem),
		.p2_req(p2_req),
		.p2_wren(p2_wren),
		.p2_ready(p2_ready),
		.p2_offset(p2_offset),
		
		.p3_address({7'h01, p3_address}),	// XenonGecko
		.p3_to_mem(p3_to_mem),
		.p3_req(p3_req),
		.p3_wren(p3_wren),
		.p3_ready(p3_ready),
		.p3_offset(p3_offset),

		.sdram_cke(sdram_cke),
		.sdram_cs_n(sdram_cs_n),
		.sdram_wre_n(sdram_wre_n),
		.sdram_cas_n(sdram_cas_n),
		.sdram_ras_n(sdram_ras_n),
		.sdram_a(sdram_a),
		.sdram_ba(sdram_ba),
		.sdram_dqm(sdram_dqm),
		.sdram_dq(sdram_dq),
		
		.init_req(init_req),
		.init_ready(init_ready),
		.init_address(init_address),
		.init_data(init_data));
//#############################################################################
		
//####### ROM #################################################################
reg ROM_ready;
always @(posedge clk_sys)
begin
	ROM_ready <= init_req;
end
assign init_ready = ROM_ready;
	PRG_ROM PRG_inst(.address(init_address[11:0]), .clock(clk_sys), .q(init_data));
//############################################################################

//####### Serial Module #######################################################
wire serial_en;
wire[7:0] from_serial;
wire uart_rx_int;
wire uart_tx_int;
assign serial_en = &IO_address[15:1];	//0xFFFE - 0xFFFF

serial serial_inst(
		.clk(clk_sys),
		.reset(rst),
		.A(IO_address[0]),
		.CE(serial_en),
		.WREN(IO_wren),
		.REN(IO_ren),
		.rx(RXD),
		.tx(TXD),
		.rx_int(uart_rx_int),
		.tx_int(uart_tx_int),
		.to_CPU(from_serial),
		.from_CPU(from_cpu[7:0]));
//#############################################################################

//####### keyboard Module #####################################################
wire keyboard_en;
wire[7:0] from_keyboard;
wire kb_rx_int;
assign keyboard_en = (&IO_address[15:4]) & ~IO_address[3] & IO_address[2] & ~IO_address[1];	//0xFFF4 - 0xFFF5

keyboard keyboard_inst(
		.clk(clk_sys),
		.reset(rst),
		.A(IO_address[0]),
		.CE(keyboard_en),
		.WREN(IO_wren),
		.REN(IO_ren),
		.ps2_data_d(ps2_data_d),
		.ps2_clk_d(ps2_clk_d),
		.ps2_data_q(ps2_data_q),
		.ps2_clk_q(ps2_clk_q),
		.rx_int(kb_rx_int),
		.to_CPU(from_keyboard),
		.from_CPU(from_cpu[7:0]));
//#############################################################################

//####### Timer Module ########################################################
wire timer_en;
wire[7:0] from_timer;
wire timer_int;
logic vsync;
logic hsync; 
assign timer_en = (&IO_address[15:4]) & ~IO_address[3] & ~IO_address[2];

timer timer_inst(
		.clk(clk_sys),
		.rst(rst),
		.ce(timer_en),
		.wren(IO_wren),
		//.ren(IO_ren),
		.hsync(hsync),
		.vsync(vsync),
		.timer_int(timer_int),
		.addr(IO_address[1:0]),
		.from_cpu(from_cpu[7:0]),
		.to_cpu(from_timer));
//#############################################################################

//####### Interrupt Controller ################################################
wire intcon_en;
wire[7:0] from_intcon;
wire int_rq;
wire[2:0] int_addr;
assign intcon_en = (&IO_address[15:5]) & ~IO_address[4] & (&IO_address[3:1]);

interrupt_controller intcon_inst(
		.clk(clk_sys),
		.rst(rst),
		.ce(intcon_en),
		.wren(IO_wren),
		.in0(button_s), .in1(~hsync), .in2(~vsync), .in3(uart_rx_int), .in4(uart_tx_int), .in5(kb_rx_int), .in6(timer_int), .in7(1'b0),
		.ri_addr(IO_address[0]),
		.from_cpu(from_cpu[7:0]),
		.to_cpu(from_intcon),
		.int_addr(int_addr),
		.int_rq(int_rq));

//#############################################################################

//####### Memory Subsystem Control ############################################
wire MSC_en;
assign MSC_en = &IO_address[15:3] && ~IO_address[2] && IO_wren;
assign LED = hex_indicators[1:0];

MSC MSC_inst(
		.clk(clk_sys),
		.rst(rst),
		.wren(MSC_en),
		.A(IO_address[1:0]),
		.data(from_cpu[7:0]),
		.p1_page(),
		.p2_page(),
		.p1_reset(p_cache_rst),
		.p1_prefetch(p_cache_prefetch),
		.p2_reset(d_cache_rst),
		.p2_flush(d_cache_flush),
		.p2_prefetch(d_cache_prefetch),
		.p2_req(p2_req),
		.p1_req(p1_req),
		.p2_ready(p2_ready),
		.p1_ready(p1_ready));
//#############################################################################

//####### VGA Module ##########################################################
wire XG_En;
wire[15:0] from_XG;
assign XG_En = (&IO_address[15:5]) & ~IO_address[4];

XenonGecko_gen2 XG_inst(
		.clk_sys(clk_sys),		//ri and memory clock
		.clk_25(clk_25),		//VGA clock
		.clk_250(clk_250),		//HDMI clock
		.rst(rst),
		.hsync(hsync),
		.vsync(vsync),
		// CPU interface
		.ri_en(XG_En),
		.ri_wren(IO_wren),
		.ri_ren(IO_ren),
		.ri_addr(IO_address[3:0]),
		.from_cpu(from_cpu),
		.to_cpu(from_XG),
		// memory interface
		.mem_req(p3_req),
		.mem_wren(p3_wren),
		.mem_ready(p3_ready),
		.mem_offset(p3_offset),
		.mem_addr(p3_address),
		.to_mem(p3_to_mem),
		.from_mem(from_mem),
		// HDMI interface
		.tmds_r_p(tmds_r_p),
		.tmds_r_n(tmds_r_n),
		.tmds_g_p(tmds_g_p),
		.tmds_g_n(tmds_g_n),
		.tmds_b_p(tmds_b_p),
		.tmds_b_n(tmds_b_n),
		.tmds_clk_p(tmds_clk_p),
		.tmds_clk_n(tmds_clk_n));
//#############################################################################

//####### IO Multiplexer ######################################################
reg prev_XG_en;
reg prev_keyboard_en;
reg prev_serial_en;
reg prev_timer_en;
reg prev_intcon_en;

always @(posedge clk_sys)
begin
	prev_XG_en <= XG_En;
	prev_keyboard_en <= keyboard_en;
	prev_serial_en <= serial_en;
	prev_timer_en <= timer_en;
	prev_intcon_en <= intcon_en;
end

wire[7:0] m_from_XG;
wire[7:0] m_from_keyboard;
wire[7:0] m_from_serial;
wire[7:0] m_from_timer;
wire[7:0] m_from_intcon;

assign m_from_XG = from_XG[7:0] & {8{prev_XG_en}};
assign m_from_keyboard = from_keyboard & {8{prev_keyboard_en}};
assign m_from_serial = from_serial & {8{prev_serial_en}};
assign m_from_timer = from_timer & {8{prev_timer_en}};
assign m_from_intcon = from_intcon & {8{prev_intcon_en}};
assign IO_to_cpu[7:0] = m_from_XG | m_from_keyboard | m_from_serial | m_from_timer | m_from_intcon;
assign IO_to_cpu[15:8] = from_XG[15:8];
//#############################################################################

//####### CPU Core ############################################################
NeonFox CPU_inst(
		.clk(clk_sys),
		.halt(1'b0),
		.reset(rst),
		.int_rq(int_rq),	//interrupt request
		.int_addr({1'b0, int_addr}),	//interrupt address
		.p_cache_miss(p_cache_miss),	//program cache read miss
		.d_cache_read_miss(d_read_miss),	//data cache read miss
		.d_cache_write_miss(d_write_miss),	//data cache write miss
		.prg_data(prg_data),	//program data
		.prg_address(prg_address),	//program address
		.data_address(data_address),	//NOT used for IO
		.IO_address(IO_address),
		.data_out(from_cpu),	//data and IO out
		.data_in(data_to_cpu),
		.IO_in(IO_to_cpu),
		.data_wren(data_wren),	//data write enable
		.data_ren(data_ren),	//data read enable
		.IO_wren(IO_wren),	//high during IO writes
		.IO_ren(IO_ren),		//IO read enable
		.H_en(H_en),	//high byte write enable for IO and data
		.L_en(L_en)	//low byte write enable for IO and data
		);
//#############################################################################
endmodule
