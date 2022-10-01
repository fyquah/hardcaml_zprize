module store_sm (
    start,
    clear,
    clock,
    tready,
    done_,
    tvalid,
    rd_addr,
    rd_en,
    block
);

    input start;
    input clear;
    input clock;
    input tready;
    output done_;
    output tvalid;
    output [8:0] rd_addr;
    output rd_en;
    output block;

    /* signal declarations */
    wire _28;
    wire _27;
    wire _29;
    wire _25;
    wire _30;
    wire _2;
    wire [8:0] _34;
    wire _38 = 1'b0;
    wire _37 = 1'b0;
    wire _50;
    wire gnd = 1'b0;
    wire _42;
    wire _43;
    wire _36;
    wire _44;
    wire _35;
    wire _51;
    wire _5;
    reg _39;
    wire [1:0] _21 = 2'b00;
    wire [1:0] _20 = 2'b00;
    wire _8;
    wire [1:0] _80;
    wire [3:0] _48 = 4'b1000;
    wire [3:0] _46 = 4'b0000;
    wire [3:0] _45 = 4'b0000;
    wire [3:0] _58 = 4'b0000;
    wire [3:0] _55 = 4'b0001;
    wire [3:0] _56;
    wire _54;
    wire [3:0] _57;
    wire _53;
    wire [3:0] _59;
    wire [3:0] _9;
    reg [3:0] _47;
    wire _49;
    wire [1:0] _78;
    wire [9:0] _40 = 10'b1000001000;
    wire vdd = 1'b1;
    wire [9:0] _32 = 10'b0000000000;
    wire _11;
    wire [9:0] _31 = 10'b0000000000;
    wire _13;
    wire [9:0] _70 = 10'b0000000000;
    wire [9:0] _65 = 10'b0000000000;
    wire [9:0] _63 = 10'b0000000001;
    wire [9:0] _64;
    wire [9:0] _66;
    wire [9:0] _67;
    wire _62;
    wire [9:0] _68;
    wire _61;
    wire [9:0] _69;
    wire _60;
    wire [9:0] _71;
    wire [9:0] _14;
    reg [9:0] _33;
    wire _41;
    wire [1:0] _75;
    wire _16;
    wire [1:0] _76;
    wire [1:0] _26 = 2'b10;
    wire _74;
    wire [1:0] _77;
    wire [1:0] _24 = 2'b01;
    wire _73;
    wire [1:0] _79;
    wire _72;
    wire [1:0] _81;
    wire [1:0] _17;
    reg [1:0] _23;
    wire [1:0] _52 = 2'b00;
    wire _82;

    /* logic */
    assign _28 = _16 ? vdd : gnd;
    assign _27 = _23 == _26;
    assign _29 = _27 ? _28 : gnd;
    assign _25 = _23 == _24;
    assign _30 = _25 ? vdd : _29;
    assign _2 = _30;
    assign _34 = _33[8:0];
    assign _50 = _49 ? vdd : _39;
    assign _42 = _41 ? gnd : _39;
    assign _43 = _16 ? _42 : _39;
    assign _36 = _23 == _26;
    assign _44 = _36 ? _43 : _39;
    assign _35 = _23 == _24;
    assign _51 = _35 ? _50 : _44;
    assign _5 = _51;
    always @(posedge _13) begin
        if (_11)
            _39 <= _38;
        else
            _39 <= _5;
    end
    assign _8 = start;
    assign _80 = _8 ? _24 : _23;
    assign _56 = _47 + _55;
    assign _54 = _23 == _24;
    assign _57 = _54 ? _56 : _47;
    assign _53 = _23 == _52;
    assign _59 = _53 ? _58 : _57;
    assign _9 = _59;
    always @(posedge _13) begin
        if (_11)
            _47 <= _46;
        else
            _47 <= _9;
    end
    assign _49 = _47 == _48;
    assign _78 = _49 ? _26 : _23;
    assign _11 = clear;
    assign _13 = clock;
    assign _64 = _33 + _63;
    assign _66 = _41 ? _65 : _64;
    assign _67 = _16 ? _66 : _33;
    assign _62 = _23 == _26;
    assign _68 = _62 ? _67 : _33;
    assign _61 = _23 == _24;
    assign _69 = _61 ? _64 : _68;
    assign _60 = _23 == _52;
    assign _71 = _60 ? _70 : _69;
    assign _14 = _71;
    always @(posedge _13) begin
        if (_11)
            _33 <= _32;
        else
            _33 <= _14;
    end
    assign _41 = _33 == _40;
    assign _75 = _41 ? _52 : _23;
    assign _16 = tready;
    assign _76 = _16 ? _75 : _23;
    assign _74 = _23 == _26;
    assign _77 = _74 ? _76 : _23;
    assign _73 = _23 == _24;
    assign _79 = _73 ? _78 : _77;
    assign _72 = _23 == _52;
    assign _81 = _72 ? _80 : _79;
    assign _17 = _81;
    always @(posedge _13) begin
        if (_11)
            _23 <= _21;
        else
            _23 <= _17;
    end
    assign _82 = _52 == _23;

    /* aliases */

    /* output assignments */
    assign done_ = _82;
    assign tvalid = _39;
    assign rd_addr = _34;
    assign rd_en = _2;
    assign block = gnd;

endmodule
module controller (
    start,
    clear,
    clock,
    cores_done,
    output_done,
    input_done,
    done_,
    start_input,
    start_output,
    start_cores,
    first_iter,
    flip
);

    input start;
    input clear;
    input clock;
    input cores_done;
    input output_done;
    input input_done;
    output done_;
    output start_input;
    output start_output;
    output start_cores;
    output first_iter;
    output flip;

    /* signal declarations */
    wire _25;
    wire _35 = 1'b1;
    wire _36;
    wire _31;
    wire _37;
    wire _2;
    wire _51;
    wire _48;
    wire _49;
    wire _40;
    wire _50;
    wire _38;
    wire _52;
    wire START_CORES;
    wire _58;
    wire _59;
    wire _56;
    wire _55;
    wire _57;
    wire _53;
    wire _60;
    wire START_OUTPUT;
    wire _70;
    wire _68;
    wire _65;
    wire _66;
    wire gnd = 1'b0;
    wire _64;
    wire _67;
    wire _63;
    wire _69;
    wire _62;
    wire _71;
    wire START_INPUT;
    wire [2:0] _27 = 3'b000;
    wire [2:0] _26 = 3'b000;
    wire _11;
    wire [2:0] _94;
    wire [2:0] _92;
    wire [5:0] _46 = 6'b000000;
    wire [5:0] _44 = 6'b000001;
    wire vdd = 1'b1;
    wire [5:0] _42 = 6'b000000;
    wire _13;
    wire [5:0] _41 = 6'b000000;
    wire _15;
    wire [5:0] _76 = 6'b000001;
    wire [5:0] _77;
    wire [5:0] _74;
    wire _73;
    wire [5:0] _75;
    wire _72;
    wire [5:0] _78;
    wire [5:0] _16;
    reg [5:0] ITERATION;
    wire [5:0] _45;
    wire _47;
    wire [2:0] _89;
    wire [2:0] _90;
    wire [2:0] _87;
    wire _18;
    wire _20;
    wire _22;
    wire _33;
    wire _34;
    wire [2:0] _85;
    wire [2:0] _83 = 3'b100;
    wire _84;
    wire [2:0] _86;
    wire [2:0] _54 = 3'b011;
    wire _82;
    wire [2:0] _88;
    wire [2:0] _39 = 3'b010;
    wire _81;
    wire [2:0] _91;
    wire [2:0] _30 = 3'b001;
    wire _80;
    wire [2:0] _93;
    wire _79;
    wire [2:0] _95;
    wire [2:0] _23;
    reg [2:0] STATE;
    wire [2:0] _61 = 3'b000;
    wire _96;

    /* logic */
    assign _25 = START_CORES | START_OUTPUT;
    assign _36 = _34 ? _35 : gnd;
    assign _31 = STATE == _30;
    assign _37 = _31 ? _36 : gnd;
    assign _2 = _37;
    assign _51 = _34 ? vdd : gnd;
    assign _48 = _47 ? vdd : vdd;
    assign _49 = _34 ? _48 : gnd;
    assign _40 = STATE == _39;
    assign _50 = _40 ? _49 : gnd;
    assign _38 = STATE == _30;
    assign _52 = _38 ? _51 : _50;
    assign START_CORES = _52;
    assign _58 = _47 ? vdd : vdd;
    assign _59 = _34 ? _58 : gnd;
    assign _56 = _34 ? vdd : gnd;
    assign _55 = STATE == _54;
    assign _57 = _55 ? _56 : gnd;
    assign _53 = STATE == _39;
    assign _60 = _53 ? _59 : _57;
    assign START_OUTPUT = _60;
    assign _70 = _11 ? vdd : gnd;
    assign _68 = _34 ? vdd : gnd;
    assign _65 = _47 ? gnd : vdd;
    assign _66 = _34 ? _65 : gnd;
    assign _64 = STATE == _39;
    assign _67 = _64 ? _66 : gnd;
    assign _63 = STATE == _30;
    assign _69 = _63 ? _68 : _67;
    assign _62 = STATE == _61;
    assign _71 = _62 ? _70 : _69;
    assign START_INPUT = _71;
    assign _11 = start;
    assign _94 = _11 ? _30 : STATE;
    assign _92 = _34 ? _39 : STATE;
    assign _13 = clear;
    assign _15 = clock;
    assign _77 = _34 ? _76 : ITERATION;
    assign _74 = _34 ? _45 : ITERATION;
    assign _73 = STATE == _39;
    assign _75 = _73 ? _74 : ITERATION;
    assign _72 = STATE == _30;
    assign _78 = _72 ? _77 : _75;
    assign _16 = _78;
    always @(posedge _15) begin
        if (_13)
            ITERATION <= _42;
        else
            ITERATION <= _16;
    end
    assign _45 = ITERATION + _44;
    assign _47 = _45 == _46;
    assign _89 = _47 ? _54 : STATE;
    assign _90 = _34 ? _89 : STATE;
    assign _87 = _34 ? _83 : STATE;
    assign _18 = cores_done;
    assign _20 = output_done;
    assign _22 = input_done;
    assign _33 = _22 & _20;
    assign _34 = _33 & _18;
    assign _85 = _34 ? _61 : STATE;
    assign _84 = STATE == _83;
    assign _86 = _84 ? _85 : STATE;
    assign _82 = STATE == _54;
    assign _88 = _82 ? _87 : _86;
    assign _81 = STATE == _39;
    assign _91 = _81 ? _90 : _88;
    assign _80 = STATE == _30;
    assign _93 = _80 ? _92 : _91;
    assign _79 = STATE == _61;
    assign _95 = _79 ? _94 : _93;
    assign _23 = _95;
    always @(posedge _15) begin
        if (_13)
            STATE <= _27;
        else
            STATE <= _23;
    end
    assign _96 = _61 == STATE;

    /* aliases */

    /* output assignments */
    assign done_ = _96;
    assign start_input = START_INPUT;
    assign start_output = START_OUTPUT;
    assign start_cores = START_CORES;
    assign first_iter = _2;
    assign flip = _25;

endmodule
module load_sm (
    first_4step_pass,
    start,
    tvalid,
    clear,
    clock,
    done_,
    tready,
    wr_addr,
    wr_en
);

    input first_4step_pass;
    input start;
    input tvalid;
    input clear;
    input clock;
    output done_;
    output tready;
    output [8:0] wr_addr;
    output wr_en;

    /* signal declarations */
    wire _24;
    wire _3;
    wire [8:0] _28;
    wire _23;
    wire [1:0] _20 = 2'b00;
    wire [1:0] _19 = 2'b00;
    wire _7;
    wire [1:0] _61;
    wire [8:0] _56 = 9'b000000000;
    wire [8:0] _32 = 9'b000000001;
    wire [8:0] _26 = 9'b000000000;
    wire [8:0] _25 = 9'b000000000;
    wire [8:0] _36 = 9'b000000000;
    wire [8:0] _34;
    wire _31;
    wire [8:0] _35;
    wire _30;
    wire [8:0] _37;
    wire [8:0] _8;
    reg [8:0] _27;
    wire [8:0] _33;
    wire _57;
    wire [1:0] _58;
    wire _10;
    wire [1:0] _59;
    wire _52 = 1'b1;
    wire vdd = 1'b1;
    wire _42 = 1'b0;
    wire _12;
    wire _41 = 1'b0;
    wire _14;
    wire _47 = 1'b0;
    wire _44 = 1'b1;
    wire _45;
    wire _40;
    wire _46;
    wire _38;
    wire _48;
    wire _15;
    reg _43;
    wire _53;
    wire [1:0] _54;
    wire [1:0] _39 = 2'b10;
    wire _51;
    wire [1:0] _55;
    wire [1:0] _18 = 2'b01;
    wire _50;
    wire [1:0] _60;
    wire _49;
    wire [1:0] _62;
    wire [1:0] _16;
    reg [1:0] _22;
    wire [1:0] _29 = 2'b00;
    wire _63;

    /* logic */
    assign _24 = _23 & _10;
    assign _3 = first_4step_pass;
    assign _28 = _3 ? _27 : _27;
    assign _23 = _18 == _22;
    assign _7 = start;
    assign _61 = _7 ? _18 : _22;
    assign _34 = _10 ? _33 : _27;
    assign _31 = _22 == _18;
    assign _35 = _31 ? _34 : _27;
    assign _30 = _22 == _29;
    assign _37 = _30 ? _36 : _35;
    assign _8 = _37;
    always @(posedge _14) begin
        if (_12)
            _27 <= _26;
        else
            _27 <= _8;
    end
    assign _33 = _27 + _32;
    assign _57 = _33 == _56;
    assign _58 = _57 ? _39 : _22;
    assign _10 = tvalid;
    assign _59 = _10 ? _58 : _22;
    assign _12 = clear;
    assign _14 = clock;
    assign _45 = _43 + _44;
    assign _40 = _22 == _39;
    assign _46 = _40 ? _45 : _43;
    assign _38 = _22 == _29;
    assign _48 = _38 ? _47 : _46;
    assign _15 = _48;
    always @(posedge _14) begin
        if (_12)
            _43 <= _42;
        else
            _43 <= _15;
    end
    assign _53 = _43 == _52;
    assign _54 = _53 ? _29 : _22;
    assign _51 = _22 == _39;
    assign _55 = _51 ? _54 : _22;
    assign _50 = _22 == _18;
    assign _60 = _50 ? _59 : _55;
    assign _49 = _22 == _29;
    assign _62 = _49 ? _61 : _60;
    assign _16 = _62;
    always @(posedge _14) begin
        if (_12)
            _22 <= _20;
        else
            _22 <= _16;
    end
    assign _63 = _29 == _22;

    /* aliases */

    /* output assignments */
    assign done_ = _63;
    assign tready = _23;
    assign wr_addr = _28;
    assign wr_en = _24;

endmodule
module ctrl (
    first_4step_pass,
    start,
    clear,
    clock,
    first_iter,
    done_,
    i,
    j,
    k,
    m,
    addr1,
    addr2,
    omegas0,
    omegas1,
    omegas2,
    omegas3,
    omegas4,
    omegas5,
    omegas6,
    start_twiddles,
    first_stage,
    last_stage,
    twiddle_stage,
    valid,
    index,
    read_write_enable,
    flip
);

    input first_4step_pass;
    input start;
    input clear;
    input clock;
    input first_iter;
    output done_;
    output [3:0] i;
    output [8:0] j;
    output [8:0] k;
    output [8:0] m;
    output [8:0] addr1;
    output [8:0] addr2;
    output [63:0] omegas0;
    output [63:0] omegas1;
    output [63:0] omegas2;
    output [63:0] omegas3;
    output [63:0] omegas4;
    output [63:0] omegas5;
    output [63:0] omegas6;
    output start_twiddles;
    output first_stage;
    output last_stage;
    output twiddle_stage;
    output valid;
    output [3:0] index;
    output read_write_enable;
    output flip;

    /* signal declarations */
    wire _59;
    wire _52;
    wire _60;
    wire _1;
    wire _64;
    wire _65;
    wire _62;
    wire _66;
    wire _3;
    wire _73 = 1'b0;
    wire _72 = 1'b0;
    wire _92 = 1'b0;
    wire _93;
    wire _89 = 1'b1;
    wire _90;
    wire _81 = 1'b0;
    wire _79 = 1'b0;
    wire [3:0] _77 = 4'b0110;
    wire _78;
    wire _80;
    wire _82;
    wire _71;
    wire _83;
    wire _69;
    wire _91;
    wire _68;
    wire _94;
    wire _6;
    reg _74;
    wire _100 = 1'b0;
    wire _99 = 1'b0;
    wire _120 = 1'b0;
    wire _121;
    wire _115 = 1'b1;
    wire _116;
    wire _117;
    wire _118;
    wire _105 = 1'b0;
    wire _106;
    wire _102 = 1'b0;
    wire _103;
    wire _98;
    wire _104;
    wire _97;
    wire _107;
    wire _96;
    wire _119;
    wire _95;
    wire _122;
    wire _8;
    reg _101;
    wire _127 = 1'b0;
    wire _126 = 1'b0;
    wire _141 = 1'b0;
    wire _142;
    wire _136 = 1'b1;
    wire _135 = 1'b0;
    wire _137;
    wire _133;
    wire [3:0] _131 = 4'b1000;
    wire _132;
    wire _134;
    wire _138;
    wire _139;
    wire gnd = 1'b0;
    wire _129;
    wire _125;
    wire _130;
    wire _124;
    wire _140;
    wire _123;
    wire _143;
    wire _10;
    reg _128;
    wire _147 = 1'b0;
    wire _146 = 1'b0;
    wire _152 = 1'b1;
    wire _153;
    wire _149 = 1'b0;
    wire _150;
    wire _145;
    wire _151;
    wire _144;
    wire _154;
    wire _12;
    reg _148;
    wire _188 = 1'b0;
    wire _187 = 1'b0;
    wire _184 = 1'b1;
    wire _185;
    wire _180 = 1'b1;
    wire _181;
    wire _182;
    wire _159 = 1'b1;
    wire _160;
    wire _158 = 1'b0;
    wire _157;
    wire _161;
    wire _156;
    wire _183;
    wire _155;
    wire _186;
    wire _14;
    reg _189;
    wire [63:0] _198 = 64'b1001010110000011011011011110011100001111001100011100101111111010;
    wire [63:0] _197 = 64'b0011101110101011111110001010011100001011100100000001011011010111;
    wire [63:0] _196 = 64'b1111111111111111111111111111110111111111111111110000000000000010;
    wire [63:0] _195 = 64'b0000000000000001111111111111111111111111111111100000000000000000;
    wire [63:0] _194 = 64'b1111111111111111111111111111101100000000000000000000000000000101;
    wire [63:0] _193 = 64'b1111111111111111111111111110111100000000000000000000000000000001;
    wire [63:0] _192 = 64'b0000000000000000000000001111111111111111111111111111111100000000;
    wire [63:0] _191 = 64'b1111111111111110111111111111111100000000000000000000000000000001;
    wire [63:0] _190 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    reg [63:0] _199;
    wire [63:0] _208 = 64'b0000001111101000110111111101001001001110100011100111100000011111;
    wire [63:0] _207 = 64'b0000000000000100000000000000001111111111111111000000000000000000;
    wire [63:0] _206 = 64'b1111111111111111111111111111111011111111111000000000000000000001;
    wire [63:0] _205 = 64'b0000000000000000000001000000000000000000000000000000000000000000;
    wire [63:0] _204 = 64'b0000000000001111111111111111111111111111111100000000000000000000;
    wire [63:0] _203 = 64'b1111111111111111111111101111111100000000000000000000000100000001;
    wire [63:0] _202 = 64'b1111111111111110111111111111111100000000000000000000000000000001;
    wire [63:0] _201 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    wire [63:0] _200 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    reg [63:0] _209;
    wire [63:0] _218 = 64'b1100100001000011111100010110001010010100011000001011010101010001;
    wire [63:0] _217 = 64'b1100001011011110110100010111001001000011011101011110000100101110;
    wire [63:0] _216 = 64'b0000000000000000000000011111111111111101111111111111111000000000;
    wire [63:0] _215 = 64'b0000000000000000000000000000000000000000000000000000000000001000;
    wire [63:0] _214 = 64'b0000000000000000000000000000000000000000000000000000000001000000;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000001000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000001000000000000000000000000;
    wire [63:0] _211 = 64'b0000000000000001000000000000000000000000000000000000000000000000;
    wire [63:0] _210 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    reg [63:0] _219;
    wire [63:0] _228 = 64'b1111100000000000000001111111111100001000000000000000000000000001;
    wire [63:0] _227 = 64'b0000000000000000000000001000000000000000000000000000000000000000;
    wire [63:0] _226 = 64'b0000000000000000001111111111111111111111111111111100000000000000;
    wire [63:0] _225 = 64'b1110111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _224 = 64'b1111111111111111111111111111111011111111000000000000000000000001;
    wire [63:0] _223 = 64'b0000000000000001000000000000000000000000000000000000000000000000;
    wire [63:0] _222 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    wire [63:0] _221 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _220 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    reg [63:0] _229;
    wire [63:0] _238 = 64'b1011101000100101111010110101110011010001100101110000101011101011;
    wire [63:0] _237 = 64'b0000001111101000110111111101001001001110100011100111100000011111;
    wire [63:0] _236 = 64'b0000000000000100000000000000001111111111111111000000000000000000;
    wire [63:0] _235 = 64'b1111111111111111111111111111111011111111111000000000000000000001;
    wire [63:0] _234 = 64'b0000000000000000000001000000000000000000000000000000000000000000;
    wire [63:0] _233 = 64'b0000000000001111111111111111111111111111111100000000000000000000;
    wire [63:0] _232 = 64'b1111111111111111111111101111111100000000000000000000000100000001;
    wire [63:0] _231 = 64'b1111111111111110111111111111111100000000000000000000000000000001;
    wire [63:0] _230 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    reg [63:0] _239;
    wire [63:0] _248 = 64'b1011111101111001000101000011110011100110000011001010100101100110;
    wire [63:0] _247 = 64'b1111100000000000000001111111111100001000000000000000000000000001;
    wire [63:0] _246 = 64'b0000000000000000000000001000000000000000000000000000000000000000;
    wire [63:0] _245 = 64'b0000000000000000001111111111111111111111111111111100000000000000;
    wire [63:0] _244 = 64'b1110111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _243 = 64'b1111111111111111111111111111111011111111000000000000000000000001;
    wire [63:0] _242 = 64'b0000000000000001000000000000000000000000000000000000000000000000;
    wire [63:0] _241 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    wire [63:0] _240 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    reg [63:0] _249;
    wire [63:0] _258 = 64'b0001100100000101110100000010101001011100010000010001111101001110;
    wire [63:0] _257 = 64'b1011111101111001000101000011110011100110000011001010100101100110;
    wire [63:0] _256 = 64'b1111100000000000000001111111111100001000000000000000000000000001;
    wire [63:0] _255 = 64'b0000000000000000000000001000000000000000000000000000000000000000;
    wire [63:0] _254 = 64'b0000000000000000001111111111111111111111111111111100000000000000;
    wire [63:0] _253 = 64'b1110111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _252 = 64'b1111111111111111111111111111111011111111000000000000000000000001;
    wire [63:0] _251 = 64'b0000000000000001000000000000000000000000000000000000000000000000;
    wire [63:0] _250 = 64'b1111111111111111111111111111111100000000000000000000000000000000;
    reg [63:0] _259;
    wire [8:0] _264 = 9'b000000000;
    wire [8:0] _263 = 9'b000000000;
    wire [8:0] _273 = 9'b000000000;
    wire [8:0] _274;
    wire [8:0] _270;
    wire [8:0] _268 = 9'b000000001;
    wire [8:0] _269;
    wire [8:0] _271;
    wire [8:0] _266;
    wire _262;
    wire [8:0] _267;
    wire _261;
    wire [8:0] _272;
    wire _260;
    wire [8:0] _275;
    wire [8:0] _24;
    reg [8:0] _265;
    wire _365 = 1'b0;
    wire _375 = 1'b0;
    wire _376;
    wire _370 = 1'b1;
    wire _371;
    wire _372;
    wire _373;
    wire _367 = 1'b1;
    wire _368;
    wire _364;
    wire _369;
    wire _363;
    wire _374;
    wire [2:0] _48 = 3'b000;
    wire [2:0] _47 = 3'b000;
    wire [2:0] _360;
    wire [2:0] _357;
    wire [2:0] _358;
    wire [2:0] _353;
    wire [2:0] _354;
    wire [2:0] _355;
    wire [8:0] _87 = 9'b111111111;
    wire [8:0] _85 = 9'b000000000;
    wire [8:0] _84 = 9'b000000000;
    wire [8:0] _333 = 9'b000000001;
    wire [8:0] _334;
    wire [8:0] _329;
    wire [8:0] _330;
    wire [8:0] _331;
    wire [8:0] _324 = 9'b000000000;
    wire _31;
    wire [8:0] _325;
    wire [8:0] _172 = 9'b000000000;
    wire [8:0] _171 = 9'b000000000;
    wire [8:0] _304 = 9'b000000000;
    wire [8:0] _305;
    wire [8:0] _301;
    wire [8:0] _168 = 9'b000000000;
    wire [8:0] _167 = 9'b000000000;
    wire [8:0] _280 = 9'b000000001;
    wire [8:0] _281;
    wire _175 = 1'b0;
    wire [7:0] _174;
    wire [8:0] _176;
    wire [8:0] _278;
    wire _277;
    wire [8:0] _279;
    wire _276;
    wire [8:0] _282;
    wire [8:0] _32;
    reg [8:0] m_0;
    wire [8:0] _165 = 9'b000000001;
    wire [8:0] _163 = 9'b000000000;
    wire [8:0] _162 = 9'b000000000;
    wire [8:0] _293 = 9'b000000000;
    wire [8:0] _294;
    wire [8:0] _289 = 9'b000000000;
    wire [8:0] _178 = 9'b000000000;
    wire _179;
    wire [8:0] _290;
    wire [8:0] _291;
    wire [8:0] _286 = 9'b000000000;
    wire [8:0] _287;
    wire _285;
    wire [8:0] _288;
    wire _284;
    wire [8:0] _292;
    wire _283;
    wire [8:0] _295;
    wire [8:0] _33;
    reg [8:0] j_0;
    wire [8:0] _166;
    wire _170;
    wire [8:0] _302;
    wire [8:0] _299;
    wire _298;
    wire [8:0] _300;
    wire _297;
    wire [8:0] _303;
    wire _296;
    wire [8:0] _306;
    wire [8:0] _34;
    reg [8:0] k_0;
    wire [8:0] _177;
    wire [8:0] _323;
    wire [3:0] _113 = 4'b1001;
    wire [3:0] _111 = 4'b0001;
    wire [3:0] _109 = 4'b0000;
    wire [3:0] _108 = 4'b0000;
    wire [3:0] _311 = 4'b0000;
    wire _36;
    wire [3:0] _312;
    wire [3:0] _309;
    wire _308;
    wire [3:0] _310;
    wire _307;
    wire [3:0] _313;
    wire [3:0] _37;
    reg [3:0] i_0;
    wire [3:0] _112;
    wire _114;
    wire [8:0] _326;
    wire [8:0] _327;
    wire [8:0] _320 = 9'b000000000;
    wire [8:0] _318 = 9'b000000001;
    wire [8:0] _319;
    wire [8:0] _321;
    wire _317;
    wire [8:0] _322;
    wire _316;
    wire [8:0] _328;
    wire _315;
    wire [8:0] _332;
    wire _314;
    wire [8:0] _335;
    wire [8:0] _38;
    reg [8:0] _86;
    wire _88;
    wire [2:0] _351;
    wire [3:0] _75 = 4'b1000;
    wire vdd = 1'b1;
    wire [3:0] _55 = 4'b0000;
    wire _40;
    wire [3:0] _54 = 4'b0000;
    wire _42;
    wire [3:0] _341 = 4'b0000;
    wire [3:0] _57 = 4'b1000;
    wire _58;
    wire [3:0] _342;
    wire [3:0] _338 = 4'b0001;
    wire [3:0] _339;
    wire _337;
    wire [3:0] _340;
    wire _336;
    wire [3:0] _343;
    wire [3:0] _43;
    reg [3:0] _56;
    wire _76;
    wire [2:0] _349;
    wire [2:0] _70 = 3'b100;
    wire _348;
    wire [2:0] _350;
    wire [2:0] _63 = 3'b011;
    wire _347;
    wire [2:0] _352;
    wire [2:0] _51 = 3'b010;
    wire _346;
    wire [2:0] _356;
    wire [2:0] _61 = 3'b001;
    wire _345;
    wire [2:0] _359;
    wire [2:0] _67 = 3'b000;
    wire _344;
    wire [2:0] _361;
    wire [2:0] _44;
    reg [2:0] STATE;
    wire _362;
    wire _377;
    wire _45;
    reg _366;

    /* logic */
    assign _59 = _58 ? vdd : gnd;
    assign _52 = STATE == _51;
    assign _60 = _52 ? _59 : gnd;
    assign _1 = _60;
    assign _64 = STATE == _63;
    assign _65 = _64 ? vdd : gnd;
    assign _62 = STATE == _61;
    assign _66 = _62 ? vdd : _65;
    assign _3 = _66;
    assign _93 = _36 ? _92 : _74;
    assign _90 = _88 ? _89 : _74;
    assign _78 = _56 == _77;
    assign _80 = _78 ? _79 : _74;
    assign _82 = _76 ? _81 : _80;
    assign _71 = STATE == _70;
    assign _83 = _71 ? _82 : _74;
    assign _69 = STATE == _63;
    assign _91 = _69 ? _90 : _83;
    assign _68 = STATE == _67;
    assign _94 = _68 ? _93 : _91;
    assign _6 = _94;
    always @(posedge _42) begin
        if (_40)
            _74 <= _73;
        else
            _74 <= _6;
    end
    assign _121 = _36 ? _120 : _101;
    assign _116 = _31 ? _115 : _101;
    assign _117 = _114 ? _116 : _101;
    assign _118 = _58 ? _117 : _101;
    assign _106 = _88 ? _105 : _101;
    assign _103 = _76 ? _102 : _101;
    assign _98 = STATE == _70;
    assign _104 = _98 ? _103 : _101;
    assign _97 = STATE == _63;
    assign _107 = _97 ? _106 : _104;
    assign _96 = STATE == _51;
    assign _119 = _96 ? _118 : _107;
    assign _95 = STATE == _67;
    assign _122 = _95 ? _121 : _119;
    assign _8 = _122;
    always @(posedge _42) begin
        if (_40)
            _101 <= _100;
        else
            _101 <= _8;
    end
    assign _142 = _36 ? _141 : _128;
    assign _137 = _31 ? _136 : _135;
    assign _133 = ~ _31;
    assign _132 = _112 == _131;
    assign _134 = _132 ? _133 : _128;
    assign _138 = _114 ? _137 : _134;
    assign _139 = _58 ? _138 : _128;
    assign _129 = _76 ? gnd : _128;
    assign _125 = STATE == _70;
    assign _130 = _125 ? _129 : _128;
    assign _124 = STATE == _51;
    assign _140 = _124 ? _139 : _130;
    assign _123 = STATE == _67;
    assign _143 = _123 ? _142 : _140;
    assign _10 = _143;
    always @(posedge _42) begin
        if (_40)
            _128 <= _127;
        else
            _128 <= _10;
    end
    assign _153 = _36 ? _152 : _148;
    assign _150 = _58 ? _149 : _148;
    assign _145 = STATE == _51;
    assign _151 = _145 ? _150 : _148;
    assign _144 = STATE == _67;
    assign _154 = _144 ? _153 : _151;
    assign _12 = _154;
    always @(posedge _42) begin
        if (_40)
            _148 <= _147;
        else
            _148 <= _12;
    end
    assign _185 = _36 ? _184 : _158;
    assign _181 = _179 ? _158 : _180;
    assign _182 = _170 ? _181 : _158;
    assign _160 = _58 ? _159 : _158;
    assign _157 = STATE == _51;
    assign _161 = _157 ? _160 : _158;
    assign _156 = STATE == _61;
    assign _183 = _156 ? _182 : _161;
    assign _155 = STATE == _67;
    assign _186 = _155 ? _185 : _183;
    assign _14 = _186;
    always @(posedge _42) begin
        if (_40)
            _189 <= _188;
        else
            _189 <= _14;
    end
    always @* begin
        case (i_0)
        0: _199 <= _190;
        1: _199 <= _191;
        2: _199 <= _192;
        3: _199 <= _193;
        4: _199 <= _194;
        5: _199 <= _195;
        6: _199 <= _196;
        7: _199 <= _197;
        default: _199 <= _198;
        endcase
    end
    always @* begin
        case (i_0)
        0: _209 <= _200;
        1: _209 <= _201;
        2: _209 <= _202;
        3: _209 <= _203;
        4: _209 <= _204;
        5: _209 <= _205;
        6: _209 <= _206;
        7: _209 <= _207;
        default: _209 <= _208;
        endcase
    end
    always @* begin
        case (i_0)
        0: _219 <= _210;
        1: _219 <= _211;
        2: _219 <= _212;
        3: _219 <= _213;
        4: _219 <= _214;
        5: _219 <= _215;
        6: _219 <= _216;
        7: _219 <= _217;
        default: _219 <= _218;
        endcase
    end
    always @* begin
        case (i_0)
        0: _229 <= _220;
        1: _229 <= _221;
        2: _229 <= _222;
        3: _229 <= _223;
        4: _229 <= _224;
        5: _229 <= _225;
        6: _229 <= _226;
        7: _229 <= _227;
        default: _229 <= _228;
        endcase
    end
    always @* begin
        case (i_0)
        0: _239 <= _230;
        1: _239 <= _231;
        2: _239 <= _232;
        3: _239 <= _233;
        4: _239 <= _234;
        5: _239 <= _235;
        6: _239 <= _236;
        7: _239 <= _237;
        default: _239 <= _238;
        endcase
    end
    always @* begin
        case (i_0)
        0: _249 <= _240;
        1: _249 <= _241;
        2: _249 <= _242;
        3: _249 <= _243;
        4: _249 <= _244;
        5: _249 <= _245;
        6: _249 <= _246;
        7: _249 <= _247;
        default: _249 <= _248;
        endcase
    end
    always @* begin
        case (i_0)
        0: _259 <= _250;
        1: _259 <= _251;
        2: _259 <= _252;
        3: _259 <= _253;
        4: _259 <= _254;
        5: _259 <= _255;
        6: _259 <= _256;
        7: _259 <= _257;
        default: _259 <= _258;
        endcase
    end
    assign _274 = _36 ? _273 : _265;
    assign _270 = _179 ? _269 : _177;
    assign _269 = _265 + _268;
    assign _271 = _170 ? _270 : _269;
    assign _266 = _58 ? _177 : _265;
    assign _262 = STATE == _51;
    assign _267 = _262 ? _266 : _265;
    assign _261 = STATE == _61;
    assign _272 = _261 ? _271 : _267;
    assign _260 = STATE == _67;
    assign _275 = _260 ? _274 : _272;
    assign _24 = _275;
    always @(posedge _42) begin
        if (_40)
            _265 <= _264;
        else
            _265 <= _24;
    end
    assign _376 = _36 ? _375 : _366;
    assign _371 = _31 ? _366 : _370;
    assign _372 = _114 ? _371 : _366;
    assign _373 = _58 ? _372 : _366;
    assign _368 = _76 ? _367 : _366;
    assign _364 = STATE == _70;
    assign _369 = _364 ? _368 : _366;
    assign _363 = STATE == _51;
    assign _374 = _363 ? _373 : _369;
    assign _360 = _36 ? _61 : STATE;
    assign _357 = _179 ? _51 : STATE;
    assign _358 = _170 ? _357 : STATE;
    assign _353 = _31 ? _63 : _67;
    assign _354 = _114 ? _353 : _61;
    assign _355 = _58 ? _354 : STATE;
    assign _334 = _36 ? _333 : _86;
    assign _329 = _177 + m_0;
    assign _330 = _179 ? _319 : _329;
    assign _331 = _170 ? _330 : _319;
    assign _31 = first_4step_pass;
    assign _325 = _31 ? _324 : _323;
    assign _305 = _36 ? _304 : k_0;
    assign _301 = _179 ? k_0 : _177;
    assign _281 = _36 ? _280 : m_0;
    assign _174 = m_0[7:0];
    assign _176 = { _174, _175 };
    assign _278 = _58 ? _176 : m_0;
    assign _277 = STATE == _51;
    assign _279 = _277 ? _278 : m_0;
    assign _276 = STATE == _67;
    assign _282 = _276 ? _281 : _279;
    assign _32 = _282;
    always @(posedge _42) begin
        if (_40)
            m_0 <= _168;
        else
            m_0 <= _32;
    end
    assign _294 = _36 ? _293 : j_0;
    assign _179 = _177 == _178;
    assign _290 = _179 ? _166 : _289;
    assign _291 = _170 ? _290 : _166;
    assign _287 = _58 ? _286 : j_0;
    assign _285 = STATE == _51;
    assign _288 = _285 ? _287 : j_0;
    assign _284 = STATE == _61;
    assign _292 = _284 ? _291 : _288;
    assign _283 = STATE == _67;
    assign _295 = _283 ? _294 : _292;
    assign _33 = _295;
    always @(posedge _42) begin
        if (_40)
            j_0 <= _163;
        else
            j_0 <= _33;
    end
    assign _166 = j_0 + _165;
    assign _170 = _166 == m_0;
    assign _302 = _170 ? _301 : k_0;
    assign _299 = _58 ? _177 : k_0;
    assign _298 = STATE == _51;
    assign _300 = _298 ? _299 : k_0;
    assign _297 = STATE == _61;
    assign _303 = _297 ? _302 : _300;
    assign _296 = STATE == _67;
    assign _306 = _296 ? _305 : _303;
    assign _34 = _306;
    always @(posedge _42) begin
        if (_40)
            k_0 <= _172;
        else
            k_0 <= _34;
    end
    assign _177 = k_0 + _176;
    assign _323 = _177 + _176;
    assign _36 = start;
    assign _312 = _36 ? _311 : i_0;
    assign _309 = _58 ? _112 : i_0;
    assign _308 = STATE == _51;
    assign _310 = _308 ? _309 : i_0;
    assign _307 = STATE == _67;
    assign _313 = _307 ? _312 : _310;
    assign _37 = _313;
    always @(posedge _42) begin
        if (_40)
            i_0 <= _109;
        else
            i_0 <= _37;
    end
    assign _112 = i_0 + _111;
    assign _114 = _112 == _113;
    assign _326 = _114 ? _325 : _323;
    assign _327 = _58 ? _326 : _86;
    assign _319 = _86 + _318;
    assign _321 = _88 ? _320 : _319;
    assign _317 = STATE == _63;
    assign _322 = _317 ? _321 : _86;
    assign _316 = STATE == _51;
    assign _328 = _316 ? _327 : _322;
    assign _315 = STATE == _61;
    assign _332 = _315 ? _331 : _328;
    assign _314 = STATE == _67;
    assign _335 = _314 ? _334 : _332;
    assign _38 = _335;
    always @(posedge _42) begin
        if (_40)
            _86 <= _85;
        else
            _86 <= _38;
    end
    assign _88 = _86 == _87;
    assign _351 = _88 ? _70 : STATE;
    assign _40 = clear;
    assign _42 = clock;
    assign _58 = _56 == _57;
    assign _342 = _58 ? _341 : _339;
    assign _339 = _56 + _338;
    assign _337 = STATE == _70;
    assign _340 = _337 ? _339 : _56;
    assign _336 = STATE == _51;
    assign _343 = _336 ? _342 : _340;
    assign _43 = _343;
    always @(posedge _42) begin
        if (_40)
            _56 <= _55;
        else
            _56 <= _43;
    end
    assign _76 = _56 == _75;
    assign _349 = _76 ? _67 : STATE;
    assign _348 = STATE == _70;
    assign _350 = _348 ? _349 : STATE;
    assign _347 = STATE == _63;
    assign _352 = _347 ? _351 : _350;
    assign _346 = STATE == _51;
    assign _356 = _346 ? _355 : _352;
    assign _345 = STATE == _61;
    assign _359 = _345 ? _358 : _356;
    assign _344 = STATE == _67;
    assign _361 = _344 ? _360 : _359;
    assign _44 = _361;
    always @(posedge _42) begin
        if (_40)
            STATE <= _48;
        else
            STATE <= _44;
    end
    assign _362 = STATE == _67;
    assign _377 = _362 ? _376 : _374;
    assign _45 = _377;
    always @(posedge _42) begin
        if (_40)
            _366 <= vdd;
        else
            _366 <= _45;
    end

    /* aliases */

    /* output assignments */
    assign done_ = _366;
    assign i = i_0;
    assign j = j_0;
    assign k = k_0;
    assign m = m_0;
    assign addr1 = _265;
    assign addr2 = _86;
    assign omegas0 = _259;
    assign omegas1 = _249;
    assign omegas2 = _239;
    assign omegas3 = _229;
    assign omegas4 = _219;
    assign omegas5 = _209;
    assign omegas6 = _199;
    assign start_twiddles = _189;
    assign first_stage = _148;
    assign last_stage = _128;
    assign twiddle_stage = _101;
    assign valid = _74;
    assign index = _56;
    assign read_write_enable = _3;
    assign flip = _1;

endmodule
module twdl (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    clock,
    start_twiddles,
    w
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input clock;
    input start_twiddles;
    output [63:0] w;

    /* signal declarations */
    wire [63:0] _36 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _35 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _87 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _83 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _84;
    wire [64:0] _80 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _77 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _76 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _73 = 33'b000000000000000000000000000000000;
    wire [64:0] _74;
    wire [31:0] _70 = 32'b00000000000000000000000000000000;
    wire [31:0] _69;
    wire [63:0] _71;
    wire [64:0] _72;
    wire [64:0] _75;
    reg [64:0] _78;
    wire [64:0] _67 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _66 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _63 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _61;
    wire [64:0] _62;
    wire [64:0] _64;
    wire [31:0] _57;
    wire [32:0] _56 = 33'b000000000000000000000000000000000;
    wire [64:0] _58;
    wire [127:0] _52 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _51 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _49 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _48 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _46 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _45 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _42 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _41 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _2;
    reg [63:0] _43;
    wire [63:0] _39 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    reg [63:0] _40;
    wire [127:0] _44;
    reg [127:0] _47;
    reg [127:0] _50;
    reg [127:0] _53;
    wire [63:0] _54;
    wire gnd = 1'b0;
    wire [64:0] _55;
    wire [64:0] _59;
    wire _60;
    wire [64:0] _65;
    reg [64:0] _68;
    wire [64:0] _79;
    wire _81;
    wire _82;
    wire [64:0] _85;
    wire [63:0] _86;
    reg [63:0] _89;
    wire [63:0] _4;
    wire [63:0] _6;
    wire [63:0] _8;
    wire [63:0] _10;
    wire [63:0] _12;
    wire [63:0] _14;
    wire vdd = 1'b1;
    wire [2:0] _23 = 3'b000;
    wire [2:0] _22 = 3'b000;
    wire _16;
    wire [2:0] _32 = 3'b000;
    wire [2:0] _29 = 3'b001;
    wire [2:0] _30;
    wire [2:0] _26 = 3'b110;
    wire _27;
    wire _28;
    wire [2:0] _31;
    wire [2:0] _33;
    wire [2:0] _17;
    reg [2:0] _25;
    reg [63:0] _90;
    wire _19;
    wire [63:0] _92;
    wire [63:0] _20;
    reg [63:0] _37;

    /* logic */
    assign _84 = _79 - _83;
    assign _74 = { _73, _69 };
    assign _69 = _53[95:64];
    assign _71 = { _69, _70 };
    assign _72 = { gnd, _71 };
    assign _75 = _72 - _74;
    always @(posedge _16) begin
        _78 <= _75;
    end
    assign _61 = _59[63:0];
    assign _62 = { gnd, _61 };
    assign _64 = _62 - _63;
    assign _57 = _53[127:96];
    assign _58 = { _56, _57 };
    assign _2 = omegas6;
    always @(posedge _16) begin
        _43 <= _2;
    end
    always @(posedge _16) begin
        _40 <= _37;
    end
    assign _44 = _40 * _43;
    always @(posedge _16) begin
        _47 <= _44;
    end
    always @(posedge _16) begin
        _50 <= _47;
    end
    always @(posedge _16) begin
        _53 <= _50;
    end
    assign _54 = _53[63:0];
    assign _55 = { gnd, _54 };
    assign _59 = _55 - _58;
    assign _60 = _59[64:64];
    assign _65 = _60 ? _64 : _59;
    always @(posedge _16) begin
        _68 <= _65;
    end
    assign _79 = _68 + _78;
    assign _81 = _79 < _80;
    assign _82 = ~ _81;
    assign _85 = _82 ? _84 : _79;
    assign _86 = _85[63:0];
    always @(posedge _16) begin
        _89 <= _86;
    end
    assign _4 = omegas5;
    assign _6 = omegas4;
    assign _8 = omegas3;
    assign _10 = omegas2;
    assign _12 = omegas1;
    assign _14 = omegas0;
    assign _16 = clock;
    assign _30 = _25 + _29;
    assign _27 = _25 == _26;
    assign _28 = ~ _27;
    assign _31 = _28 ? _30 : _25;
    assign _33 = _19 ? _32 : _31;
    assign _17 = _33;
    always @(posedge _16) begin
        _25 <= _17;
    end
    always @* begin
        case (_25)
        0: _90 <= _14;
        1: _90 <= _12;
        2: _90 <= _10;
        3: _90 <= _8;
        4: _90 <= _6;
        5: _90 <= _4;
        default: _90 <= _89;
        endcase
    end
    assign _19 = start_twiddles;
    assign _92 = _19 ? _91 : _90;
    assign _20 = _92;
    always @(posedge _16) begin
        _37 <= _20;
    end

    /* aliases */

    /* output assignments */
    assign w = _37;

endmodule
module dp (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b0000000000000000000000000000000000000000000000000000000000000001;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_0 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b1000010001110111011101111010001101011010001110110100010100111111;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b1010011111111000011110110001100010101001010001001001111110101011;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b1011000000010011100110100001111101100101110000111000010011000111;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b0101010011011111100101100011000010111111011110010100010100001110;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b1110111111010010011111010110001000110000011000111111011001111110;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b1010101111010000101001101110100010101010001111011000101000001110;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b1000000100101000000110100111101100000101111110011011111010101100;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_1 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b0110000100001110110000110100010100011110011001010101000110111110;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b0111101101011000001000011001001010100110011001111000100000100010;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b1100100001110111001011000010100110101111100011010010000101100110;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b1010011111111000011110110001100010101001010001001001111110101011;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b0101010011011111100101100011000010111111011110010100010100001110;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b1010101111010000101001101110100010101010001111011000101000001110;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_2 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b1100100111100100001010111111010000000110010110011110110100101010;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b1010110100100101110000000010101101010100001111010011110111010010;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b0101011010100101001110110011011000101010010001011010110000100010;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b0111101101011000001000011001001010100110011001111000100000100010;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b0101101110011010011111100001010010110001101111101000110111001010;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b1010011111111000011110110001100010101001010001001001111110101011;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b1110111111010010011111010110001000110000011000111111011001111110;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_3 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b1111111100010011111001011010101110101011101011110001111000110010;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b1101110010010010000110001010100001101101000100001111001110100011;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b0111101101011000001000011001001010100110011001111000100000100010;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b0101010011011111100101100011000010111111011110010100010100001110;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_4 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b0011111100101001100101001011110100101110000110110110110000000100;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b0111101101110110100010100011000110000010110100101111010110101110;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b0010001000000001011111010001110001000010100100001111001111110001;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b1101110010010010000110001010100001101101000100001111001110100011;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b0101011010100101001110110011011000101010010001011010110000100010;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b1100100001110111001011000010100110101111100011010010000101100110;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b1011000000010011100110100001111101100101110000111000010011000111;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_5 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b0011110001000000111101000001110010010111100000100100100100101000;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b1010010001011100001010001011011000110011010000111001000000000110;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b0111101101110110100010100011000110000010110100101111010110101110;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b1010110100100101110000000010101101010100001111010011110111010010;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b0111101101011000001000011001001010100110011001111000100000100010;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b1010011111111000011110110001100010101001010001001001111110101011;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module dp_6 (
    omegas6,
    omegas5,
    omegas4,
    omegas3,
    omegas2,
    omegas1,
    omegas0,
    twiddle_stage,
    start_twiddles,
    first_iter,
    start,
    clear,
    index,
    d2,
    valid,
    clock,
    d1,
    q1,
    q2,
    twiddle_update_q
);

    input [63:0] omegas6;
    input [63:0] omegas5;
    input [63:0] omegas4;
    input [63:0] omegas3;
    input [63:0] omegas2;
    input [63:0] omegas1;
    input [63:0] omegas0;
    input twiddle_stage;
    input start_twiddles;
    input first_iter;
    input start;
    input clear;
    input [3:0] index;
    input [63:0] d2;
    input valid;
    input clock;
    input [63:0] d1;
    output [63:0] q1;
    output [63:0] q2;
    output [63:0] twiddle_update_q;

    /* signal declarations */
    wire [63:0] _104 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _103 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _98 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _99;
    wire [64:0] _95;
    wire [64:0] _94;
    wire [64:0] _96;
    wire _97;
    wire [64:0] _100;
    wire [63:0] _101;
    wire _70 = 1'b0;
    wire _69 = 1'b0;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _61 = 1'b0;
    wire _60 = 1'b0;
    wire _58 = 1'b0;
    wire _57 = 1'b0;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _52 = 1'b0;
    wire _51 = 1'b0;
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    reg _50;
    reg _53;
    reg _56;
    reg _59;
    reg _62;
    reg _65;
    reg _68;
    reg piped_twiddle_stage;
    wire [63:0] _102;
    reg [63:0] _105;
    wire [63:0] _295 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _294 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _290 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _291;
    wire [64:0] _287 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [63:0] _282 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _281 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _277 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _278;
    wire [64:0] _274 = 65'b01111111111111111111111111111111100000000000000000000000000000001;
    wire [64:0] _271 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _270 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [32:0] _267 = 33'b000000000000000000000000000000000;
    wire [64:0] _268;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [63:0] _265;
    wire [64:0] _266;
    wire [64:0] _269;
    reg [64:0] _272;
    wire [64:0] _261 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _260 = 65'b00000000000000000000000000000000000000000000000000000000000000000;
    wire [64:0] _257 = 65'b00000000000000000000000000000000011111111111111111111111111111111;
    wire [63:0] _255;
    wire [64:0] _256;
    wire [64:0] _258;
    wire [31:0] _251;
    wire [32:0] _250 = 33'b000000000000000000000000000000000;
    wire [64:0] _252;
    wire [127:0] _246 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _245 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _243 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _242 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _240 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [127:0] _239 = 128'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _236 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _235 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _232 = 64'b0001100000010110001000110101100000110100001101000010111001000000;
    wire [63:0] _231 = 64'b0011100001111011100100010011111110101000110011010110111100101110;
    wire [63:0] _230 = 64'b0010111001000011010100111101111111001110010000011101111010101111;
    wire [63:0] _229 = 64'b0001010101000100111011110010001100110101110100010111100110010111;
    wire [63:0] _228 = 64'b0011001111011011100000000001100110101011001100010100111100000011;
    wire [63:0] _227 = 64'b1110000011101110000010011001001100010000101110111010000111100010;
    wire [63:0] _226 = 64'b1111011010110010110011111111111000100011000001101011101010101100;
    reg [63:0] twiddle_scale;
    wire [63:0] _4;
    wire _152 = 1'b0;
    wire _151 = 1'b0;
    reg _153;
    wire [63:0] _157;
    wire [63:0] _6;
    wire _145 = 1'b0;
    wire _144 = 1'b0;
    reg _146;
    wire [63:0] _150;
    wire [63:0] _8;
    wire _138 = 1'b0;
    wire _137 = 1'b0;
    reg _139;
    wire [63:0] _143;
    wire [63:0] _10;
    wire _131 = 1'b0;
    wire _130 = 1'b0;
    reg _132;
    wire [63:0] _136;
    wire [63:0] _12;
    wire _124 = 1'b0;
    wire _123 = 1'b0;
    reg _125;
    wire [63:0] _129;
    wire [63:0] _14;
    wire _117 = 1'b0;
    wire _116 = 1'b0;
    reg _118;
    wire [63:0] _122;
    wire [63:0] _16;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _18;
    reg _111;
    wire [63:0] _115;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire _20;
    reg _108;
    wire [63:0] _159;
    wire [63:0] w;
    wire [63:0] twiddle_factor;
    wire [63:0] b;
    reg [63:0] _237;
    wire [63:0] _224 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _223 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _113 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _112 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _120 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _119 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _127 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _126 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _134 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _133 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _141 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _140 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _148 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _147 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _155 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _154 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _185 = 64'b0010101010101011110010100001011101011000010101110000111110011110;
    wire [63:0] _186;
    wire [63:0] _187;
    wire [63:0] _22;
    reg [63:0] twiddle_omega6;
    wire [63:0] _188 = 64'b0011110001000000111101000001110010010111100000100100100100101000;
    wire [63:0] _189;
    wire [63:0] _190;
    wire [63:0] _23;
    reg [63:0] twiddle_omega5;
    wire [63:0] _191 = 64'b0011111100101001100101001011110100101110000110110110110000000100;
    wire [63:0] _192;
    wire [63:0] _193;
    wire [63:0] _24;
    reg [63:0] twiddle_omega4;
    wire [63:0] _194 = 64'b1111111100010011111001011010101110101011101011110001111000110010;
    wire [63:0] _195;
    wire [63:0] _196;
    wire [63:0] _25;
    reg [63:0] twiddle_omega3;
    wire [63:0] _197 = 64'b1100100111100100001010111111010000000110010110011110110100101010;
    wire [63:0] _198;
    wire [63:0] _199;
    wire [63:0] _26;
    reg [63:0] twiddle_omega2;
    wire [63:0] _200 = 64'b0110000100001110110000110100010100011110011001010101000110111110;
    wire [63:0] _201;
    wire [63:0] _202;
    wire [63:0] _27;
    reg [63:0] twiddle_omega1;
    wire [63:0] _203 = 64'b1000010001110111011101111010001101011010001110110100010100111111;
    wire _29;
    wire _31;
    wire _184;
    wire [63:0] _204;
    wire _182 = 1'b0;
    wire _181 = 1'b0;
    wire _179 = 1'b0;
    wire _178 = 1'b0;
    wire _176 = 1'b0;
    wire _175 = 1'b0;
    wire _173 = 1'b0;
    wire _172 = 1'b0;
    wire _170 = 1'b0;
    wire _169 = 1'b0;
    wire _167 = 1'b0;
    wire _166 = 1'b0;
    wire _164 = 1'b0;
    wire _163 = 1'b0;
    wire _161 = 1'b0;
    wire _33;
    wire _160 = 1'b0;
    reg _162;
    reg _165;
    reg _168;
    reg _171;
    reg _174;
    reg _177;
    reg _180;
    reg _183;
    wire [63:0] _205;
    wire [63:0] _34;
    reg [63:0] twiddle_omega0;
    wire [3:0] _219 = 4'b0000;
    wire [3:0] _218 = 4'b0000;
    wire [3:0] _216 = 4'b0000;
    wire [3:0] _215 = 4'b0000;
    wire [3:0] _36;
    reg [3:0] _217;
    reg [3:0] _220;
    reg [63:0] _221;
    wire [63:0] _213 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _212 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _38;
    reg [63:0] piped_d2;
    wire _210 = 1'b0;
    wire _209 = 1'b0;
    wire _207 = 1'b0;
    wire _206 = 1'b0;
    wire _40;
    reg _208;
    reg piped_twidle_updated_valid;
    wire [63:0] a;
    reg [63:0] _225;
    wire [127:0] _238;
    reg [127:0] _241;
    reg [127:0] _244;
    reg [127:0] _247;
    wire [63:0] _248;
    wire [64:0] _249;
    wire [64:0] _253;
    wire _254;
    wire [64:0] _259;
    reg [64:0] _262;
    wire [64:0] _273;
    wire _275;
    wire _276;
    wire [64:0] _279;
    wire [63:0] _280;
    reg [63:0] _283;
    wire [63:0] T;
    wire [64:0] _285;
    wire [63:0] _92 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _91 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _89 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _88 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _86 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _85 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _82 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _80 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _79 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _77 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _76 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [63:0] _74 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _73 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire _43;
    wire [63:0] _45;
    reg [63:0] _75;
    reg [63:0] _78;
    reg [63:0] _81;
    reg [63:0] _84;
    reg [63:0] _87;
    reg [63:0] _90;
    reg [63:0] _93;
    wire gnd = 1'b0;
    wire [64:0] _284;
    wire [64:0] _286;
    wire _288;
    wire _289;
    wire [64:0] _292;
    wire [63:0] _293;
    reg [63:0] _296;

    /* logic */
    assign _99 = _96 + _98;
    assign _95 = { gnd, T };
    assign _94 = { gnd, _93 };
    assign _96 = _94 - _95;
    assign _97 = _96[64:64];
    assign _100 = _97 ? _99 : _96;
    assign _101 = _100[63:0];
    always @(posedge _43) begin
        _50 <= _18;
    end
    always @(posedge _43) begin
        _53 <= _50;
    end
    always @(posedge _43) begin
        _56 <= _53;
    end
    always @(posedge _43) begin
        _59 <= _56;
    end
    always @(posedge _43) begin
        _62 <= _59;
    end
    always @(posedge _43) begin
        _65 <= _62;
    end
    always @(posedge _43) begin
        _68 <= _65;
    end
    always @(posedge _43) begin
        piped_twiddle_stage <= _68;
    end
    assign _102 = piped_twiddle_stage ? T : _101;
    always @(posedge _43) begin
        _105 <= _102;
    end
    assign _291 = _286 - _290;
    assign _278 = _273 - _277;
    assign _268 = { _267, _263 };
    assign _263 = _247[95:64];
    assign _265 = { _263, _264 };
    assign _266 = { gnd, _265 };
    assign _269 = _266 - _268;
    always @(posedge _43) begin
        _272 <= _269;
    end
    assign _255 = _253[63:0];
    assign _256 = { gnd, _255 };
    assign _258 = _256 - _257;
    assign _251 = _247[127:96];
    assign _252 = { _250, _251 };
    always @* begin
        case (_220)
        0: twiddle_scale <= _226;
        1: twiddle_scale <= _227;
        2: twiddle_scale <= _228;
        3: twiddle_scale <= _229;
        4: twiddle_scale <= _230;
        5: twiddle_scale <= _231;
        default: twiddle_scale <= _232;
        endcase
    end
    assign _4 = omegas6;
    always @(posedge _43) begin
        _153 <= _18;
    end
    assign _157 = _153 ? twiddle_omega6 : _4;
    assign _6 = omegas5;
    always @(posedge _43) begin
        _146 <= _18;
    end
    assign _150 = _146 ? twiddle_omega5 : _6;
    assign _8 = omegas4;
    always @(posedge _43) begin
        _139 <= _18;
    end
    assign _143 = _139 ? twiddle_omega4 : _8;
    assign _10 = omegas3;
    always @(posedge _43) begin
        _132 <= _18;
    end
    assign _136 = _132 ? twiddle_omega3 : _10;
    assign _12 = omegas2;
    always @(posedge _43) begin
        _125 <= _18;
    end
    assign _129 = _125 ? twiddle_omega2 : _12;
    assign _14 = omegas1;
    always @(posedge _43) begin
        _118 <= _18;
    end
    assign _122 = _118 ? twiddle_omega1 : _14;
    assign _16 = omegas0;
    assign _18 = twiddle_stage;
    always @(posedge _43) begin
        _111 <= _18;
    end
    assign _115 = _111 ? twiddle_omega0 : _16;
    assign _20 = start_twiddles;
    always @(posedge _43) begin
        if (_33)
            _108 <= _107;
        else
            _108 <= _20;
    end
    twdl
        twdl
        ( .clock(_43), .start_twiddles(_108), .omegas0(_115), .omegas1(_122), .omegas2(_129), .omegas3(_136), .omegas4(_143), .omegas5(_150), .omegas6(_157), .w(_159[63:0]) );
    assign w = _159;
    assign b = piped_twidle_updated_valid ? twiddle_scale : w;
    always @(posedge _43) begin
        _237 <= b;
    end
    assign _186 = _184 ? _185 : twiddle_omega6;
    assign _187 = _183 ? T : _186;
    assign _22 = _187;
    always @(posedge _43) begin
        twiddle_omega6 <= _22;
    end
    assign _189 = _184 ? _188 : twiddle_omega5;
    assign _190 = _183 ? twiddle_omega6 : _189;
    assign _23 = _190;
    always @(posedge _43) begin
        twiddle_omega5 <= _23;
    end
    assign _192 = _184 ? _191 : twiddle_omega4;
    assign _193 = _183 ? twiddle_omega5 : _192;
    assign _24 = _193;
    always @(posedge _43) begin
        twiddle_omega4 <= _24;
    end
    assign _195 = _184 ? _194 : twiddle_omega3;
    assign _196 = _183 ? twiddle_omega4 : _195;
    assign _25 = _196;
    always @(posedge _43) begin
        twiddle_omega3 <= _25;
    end
    assign _198 = _184 ? _197 : twiddle_omega2;
    assign _199 = _183 ? twiddle_omega3 : _198;
    assign _26 = _199;
    always @(posedge _43) begin
        twiddle_omega2 <= _26;
    end
    assign _201 = _184 ? _200 : twiddle_omega1;
    assign _202 = _183 ? twiddle_omega2 : _201;
    assign _27 = _202;
    always @(posedge _43) begin
        twiddle_omega1 <= _27;
    end
    assign _29 = first_iter;
    assign _31 = start;
    assign _184 = _31 & _29;
    assign _204 = _184 ? _203 : twiddle_omega0;
    assign _33 = clear;
    always @(posedge _43) begin
        if (_33)
            _162 <= _161;
        else
            _162 <= _40;
    end
    always @(posedge _43) begin
        if (_33)
            _165 <= _164;
        else
            _165 <= _162;
    end
    always @(posedge _43) begin
        if (_33)
            _168 <= _167;
        else
            _168 <= _165;
    end
    always @(posedge _43) begin
        if (_33)
            _171 <= _170;
        else
            _171 <= _168;
    end
    always @(posedge _43) begin
        if (_33)
            _174 <= _173;
        else
            _174 <= _171;
    end
    always @(posedge _43) begin
        if (_33)
            _177 <= _176;
        else
            _177 <= _174;
    end
    always @(posedge _43) begin
        if (_33)
            _180 <= _179;
        else
            _180 <= _177;
    end
    always @(posedge _43) begin
        if (_33)
            _183 <= _182;
        else
            _183 <= _180;
    end
    assign _205 = _183 ? twiddle_omega1 : _204;
    assign _34 = _205;
    always @(posedge _43) begin
        twiddle_omega0 <= _34;
    end
    assign _36 = index;
    always @(posedge _43) begin
        _217 <= _36;
    end
    always @(posedge _43) begin
        _220 <= _217;
    end
    always @* begin
        case (_220)
        0: _221 <= twiddle_omega0;
        1: _221 <= twiddle_omega1;
        2: _221 <= twiddle_omega2;
        3: _221 <= twiddle_omega3;
        4: _221 <= twiddle_omega4;
        5: _221 <= twiddle_omega5;
        default: _221 <= twiddle_omega6;
        endcase
    end
    assign _38 = d2;
    always @(posedge _43) begin
        piped_d2 <= _38;
    end
    assign _40 = valid;
    always @(posedge _43) begin
        _208 <= _40;
    end
    always @(posedge _43) begin
        piped_twidle_updated_valid <= _208;
    end
    assign a = piped_twidle_updated_valid ? _221 : piped_d2;
    always @(posedge _43) begin
        _225 <= a;
    end
    assign _238 = _225 * _237;
    always @(posedge _43) begin
        _241 <= _238;
    end
    always @(posedge _43) begin
        _244 <= _241;
    end
    always @(posedge _43) begin
        _247 <= _244;
    end
    assign _248 = _247[63:0];
    assign _249 = { gnd, _248 };
    assign _253 = _249 - _252;
    assign _254 = _253[64:64];
    assign _259 = _254 ? _258 : _253;
    always @(posedge _43) begin
        _262 <= _259;
    end
    assign _273 = _262 + _272;
    assign _275 = _273 < _274;
    assign _276 = ~ _275;
    assign _279 = _276 ? _278 : _273;
    assign _280 = _279[63:0];
    always @(posedge _43) begin
        _283 <= _280;
    end
    assign T = _283;
    assign _285 = { gnd, T };
    assign _43 = clock;
    assign _45 = d1;
    always @(posedge _43) begin
        _75 <= _45;
    end
    always @(posedge _43) begin
        _78 <= _75;
    end
    always @(posedge _43) begin
        _81 <= _78;
    end
    always @(posedge _43) begin
        _84 <= _81;
    end
    always @(posedge _43) begin
        _87 <= _84;
    end
    always @(posedge _43) begin
        _90 <= _87;
    end
    always @(posedge _43) begin
        _93 <= _90;
    end
    assign _284 = { gnd, _93 };
    assign _286 = _284 + _285;
    assign _288 = _286 < _287;
    assign _289 = ~ _288;
    assign _292 = _289 ? _291 : _286;
    assign _293 = _292[63:0];
    always @(posedge _43) begin
        _296 <= _293;
    end

    /* aliases */
    assign twiddle_factor = w;

    /* output assignments */
    assign q1 = _296;
    assign q2 = _105;
    assign twiddle_update_q = T;

endmodule
module parallel_cores (
    wr_d7,
    wr_d6,
    wr_d5,
    wr_d4,
    wr_d3,
    wr_d2,
    wr_d1,
    wr_d0,
    wr_addr,
    wr_en,
    rd_addr,
    rd_en,
    flip,
    first_4step_pass,
    first_iter,
    start,
    clear,
    clock,
    done_,
    rd_q0,
    rd_q1,
    rd_q2,
    rd_q3,
    rd_q4,
    rd_q5,
    rd_q6,
    rd_q7
);

    input [63:0] wr_d7;
    input [63:0] wr_d6;
    input [63:0] wr_d5;
    input [63:0] wr_d4;
    input [63:0] wr_d3;
    input [63:0] wr_d2;
    input [63:0] wr_d1;
    input [63:0] wr_d0;
    input [8:0] wr_addr;
    input [7:0] wr_en;
    input [8:0] rd_addr;
    input [7:0] rd_en;
    input flip;
    input first_4step_pass;
    input first_iter;
    input start;
    input clear;
    input clock;
    output done_;
    output [63:0] rd_q0;
    output [63:0] rd_q1;
    output [63:0] rd_q2;
    output [63:0] rd_q3;
    output [63:0] rd_q4;
    output [63:0] rd_q5;
    output [63:0] rd_q6;
    output [63:0] rd_q7;

    /* signal declarations */
    wire [8:0] address;
    wire write_enable;
    wire [8:0] address_0;
    wire _368;
    wire read_enable;
    wire _366;
    wire write_enable_0;
    wire _370;
    wire [131:0] _375;
    wire [63:0] _376;
    wire [8:0] _361 = 9'b000000000;
    wire [8:0] address_1;
    wire _359;
    wire write_enable_1;
    wire [63:0] _273;
    wire [63:0] _272;
    wire [63:0] _274;
    wire [63:0] _270;
    wire [63:0] _269;
    wire [63:0] q1;
    wire [63:0] _275;
    wire [8:0] address_2;
    wire write_enable_2 = 1'b0;
    wire _260;
    wire read_enable_0;
    wire [8:0] address_3;
    wire _256;
    wire read_enable_1;
    wire _254;
    wire write_enable_3;
    wire _258;
    wire [131:0] _265;
    wire [63:0] _266;
    wire [63:0] data = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_0;
    wire [8:0] _248 = 9'b000000000;
    wire _246;
    wire _245;
    wire _244;
    wire _243;
    wire _242;
    wire _241;
    wire _240;
    wire _239;
    wire _238;
    wire [8:0] _247;
    wire [8:0] address_4;
    wire write_enable_4 = 1'b0;
    wire _235;
    wire read_enable_2;
    wire [63:0] data_1;
    wire [63:0] data_2;
    wire _232;
    wire _231;
    wire _230;
    wire _229;
    wire _228;
    wire _227;
    wire _226;
    wire _225;
    wire _224;
    wire [8:0] _233;
    wire [8:0] address_5;
    wire _221;
    wire read_enable_3;
    wire _219;
    wire write_enable_5;
    wire _223;
    wire [131:0] _252;
    wire [63:0] _253;
    wire _87 = 1'b0;
    wire _86 = 1'b0;
    wire _89;
    wire _3;
    reg PHASE;
    wire [63:0] _267;
    wire [8:0] address_6;
    wire _211;
    wire read_enable_4;
    wire write_enable_6;
    wire _213;
    wire [8:0] address_7;
    wire _206;
    wire read_enable_5;
    wire _204;
    wire write_enable_7;
    wire _208;
    wire [131:0] _216;
    wire [63:0] _217;
    wire [63:0] _289;
    wire [63:0] data_3;
    wire [63:0] data_4;
    wire [63:0] data_5;
    wire [63:0] data_6;
    wire [8:0] address_8;
    wire _169;
    wire read_enable_6;
    wire _166;
    wire _167;
    wire write_enable_8;
    wire _171;
    wire [8:0] address_9;
    wire _134;
    wire read_enable_7;
    wire _131;
    wire _132;
    wire write_enable_9;
    wire _136;
    wire [131:0] _202;
    wire [63:0] _203;
    wire _98 = 1'b0;
    wire _97 = 1'b0;
    wire _290;
    wire _5;
    reg PHASE_0;
    wire [63:0] q0;
    wire _94 = 1'b0;
    wire _93 = 1'b0;
    reg _96;
    wire [63:0] _268;
    wire [191:0] _288;
    wire [63:0] _291;
    wire [63:0] data_7;
    wire [63:0] data_8;
    wire [63:0] data_9;
    wire [63:0] data_10;
    wire [8:0] address_10;
    wire _355;
    wire _354;
    wire read_enable_8;
    wire _348 = 1'b0;
    wire _347 = 1'b0;
    wire _345 = 1'b0;
    wire _344 = 1'b0;
    wire _342 = 1'b0;
    wire _341 = 1'b0;
    wire _339 = 1'b0;
    wire _338 = 1'b0;
    wire _336 = 1'b0;
    wire _335 = 1'b0;
    wire _333 = 1'b0;
    wire _332 = 1'b0;
    wire _330 = 1'b0;
    wire _329 = 1'b0;
    wire _327 = 1'b0;
    wire _326 = 1'b0;
    wire _324 = 1'b0;
    wire _323 = 1'b0;
    reg _325;
    reg _328;
    reg _331;
    reg _334;
    reg _337;
    reg _340;
    reg _343;
    reg _346;
    reg _349;
    wire _350;
    wire _321 = 1'b0;
    wire _320 = 1'b0;
    wire _318 = 1'b0;
    wire _317 = 1'b0;
    wire _315 = 1'b0;
    wire _314 = 1'b0;
    wire _312 = 1'b0;
    wire _311 = 1'b0;
    wire _309 = 1'b0;
    wire _308 = 1'b0;
    wire _306 = 1'b0;
    wire _305 = 1'b0;
    wire _303 = 1'b0;
    wire _302 = 1'b0;
    wire _300 = 1'b0;
    wire _299 = 1'b0;
    wire _297 = 1'b0;
    wire _296 = 1'b0;
    reg _298;
    reg _301;
    reg _304;
    reg _307;
    reg _310;
    reg _313;
    reg _316;
    reg _319;
    reg _322;
    wire _351;
    wire _352;
    wire write_enable_10;
    wire _357;
    wire [131:0] _364;
    wire [63:0] _365;
    wire _293 = 1'b0;
    wire _292 = 1'b0;
    wire _295;
    wire _7;
    reg PHASE_1;
    wire [63:0] _377;
    wire [8:0] address_11;
    wire write_enable_11;
    wire [8:0] address_12;
    wire _558;
    wire read_enable_9;
    wire _556;
    wire write_enable_12;
    wire _560;
    wire [131:0] _565;
    wire [63:0] _566;
    wire [8:0] _551 = 9'b000000000;
    wire [8:0] address_13;
    wire _549;
    wire write_enable_13;
    wire [63:0] _474;
    wire [63:0] _473;
    wire [63:0] _475;
    wire [63:0] _471;
    wire [63:0] _470;
    wire [63:0] q1_0;
    wire [63:0] _476;
    wire [8:0] address_14;
    wire write_enable_14 = 1'b0;
    wire _461;
    wire read_enable_10;
    wire [8:0] address_15;
    wire _457;
    wire read_enable_11;
    wire _455;
    wire write_enable_15;
    wire _459;
    wire [131:0] _466;
    wire [63:0] _467;
    wire [63:0] data_11 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_12;
    wire [8:0] _449 = 9'b000000000;
    wire _447;
    wire _446;
    wire _445;
    wire _444;
    wire _443;
    wire _442;
    wire _441;
    wire _440;
    wire _439;
    wire [8:0] _448;
    wire [8:0] address_16;
    wire write_enable_16 = 1'b0;
    wire _436;
    wire read_enable_12;
    wire [63:0] data_13;
    wire [63:0] data_14;
    wire _433;
    wire _432;
    wire _431;
    wire _430;
    wire _429;
    wire _428;
    wire _427;
    wire _426;
    wire _425;
    wire [8:0] _434;
    wire [8:0] address_17;
    wire _422;
    wire read_enable_13;
    wire _420;
    wire write_enable_17;
    wire _424;
    wire [131:0] _453;
    wire [63:0] _454;
    wire _379 = 1'b0;
    wire _378 = 1'b0;
    wire _381;
    wire _11;
    reg PHASE_2;
    wire [63:0] _468;
    wire [8:0] address_18;
    wire _412;
    wire read_enable_14;
    wire write_enable_18;
    wire _414;
    wire [8:0] address_19;
    wire _407;
    wire read_enable_15;
    wire _405;
    wire write_enable_19;
    wire _409;
    wire [131:0] _417;
    wire [63:0] _418;
    wire [63:0] _479;
    wire [63:0] data_15;
    wire [63:0] data_16;
    wire [63:0] data_17;
    wire [63:0] data_18;
    wire [8:0] address_20;
    wire _398;
    wire read_enable_16;
    wire _395;
    wire _396;
    wire write_enable_20;
    wire _400;
    wire [8:0] address_21;
    wire _391;
    wire read_enable_17;
    wire _388;
    wire _389;
    wire write_enable_21;
    wire _393;
    wire [131:0] _403;
    wire [63:0] _404;
    wire _386 = 1'b0;
    wire _385 = 1'b0;
    wire _480;
    wire _13;
    reg PHASE_3;
    wire [63:0] q0_0;
    wire _383 = 1'b0;
    wire _382 = 1'b0;
    reg _384;
    wire [63:0] _469;
    wire [191:0] _478;
    wire [63:0] _481;
    wire [63:0] data_19;
    wire [63:0] data_20;
    wire [63:0] data_21;
    wire [63:0] data_22;
    wire [8:0] address_22;
    wire _545;
    wire _544;
    wire read_enable_18;
    wire _538 = 1'b0;
    wire _537 = 1'b0;
    wire _535 = 1'b0;
    wire _534 = 1'b0;
    wire _532 = 1'b0;
    wire _531 = 1'b0;
    wire _529 = 1'b0;
    wire _528 = 1'b0;
    wire _526 = 1'b0;
    wire _525 = 1'b0;
    wire _523 = 1'b0;
    wire _522 = 1'b0;
    wire _520 = 1'b0;
    wire _519 = 1'b0;
    wire _517 = 1'b0;
    wire _516 = 1'b0;
    wire _514 = 1'b0;
    wire _513 = 1'b0;
    reg _515;
    reg _518;
    reg _521;
    reg _524;
    reg _527;
    reg _530;
    reg _533;
    reg _536;
    reg _539;
    wire _540;
    wire _511 = 1'b0;
    wire _510 = 1'b0;
    wire _508 = 1'b0;
    wire _507 = 1'b0;
    wire _505 = 1'b0;
    wire _504 = 1'b0;
    wire _502 = 1'b0;
    wire _501 = 1'b0;
    wire _499 = 1'b0;
    wire _498 = 1'b0;
    wire _496 = 1'b0;
    wire _495 = 1'b0;
    wire _493 = 1'b0;
    wire _492 = 1'b0;
    wire _490 = 1'b0;
    wire _489 = 1'b0;
    wire _487 = 1'b0;
    wire _486 = 1'b0;
    reg _488;
    reg _491;
    reg _494;
    reg _497;
    reg _500;
    reg _503;
    reg _506;
    reg _509;
    reg _512;
    wire _541;
    wire _542;
    wire write_enable_22;
    wire _547;
    wire [131:0] _554;
    wire [63:0] _555;
    wire _483 = 1'b0;
    wire _482 = 1'b0;
    wire _485;
    wire _15;
    reg PHASE_4;
    wire [63:0] _567;
    wire [8:0] address_23;
    wire write_enable_23;
    wire [8:0] address_24;
    wire _748;
    wire read_enable_19;
    wire _746;
    wire write_enable_24;
    wire _750;
    wire [131:0] _755;
    wire [63:0] _756;
    wire [8:0] _741 = 9'b000000000;
    wire [8:0] address_25;
    wire _739;
    wire write_enable_25;
    wire [63:0] _664;
    wire [63:0] _663;
    wire [63:0] _665;
    wire [63:0] _661;
    wire [63:0] _660;
    wire [63:0] q1_1;
    wire [63:0] _666;
    wire [8:0] address_26;
    wire write_enable_26 = 1'b0;
    wire _651;
    wire read_enable_20;
    wire [8:0] address_27;
    wire _647;
    wire read_enable_21;
    wire _645;
    wire write_enable_27;
    wire _649;
    wire [131:0] _656;
    wire [63:0] _657;
    wire [63:0] data_23 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_24;
    wire [8:0] _639 = 9'b000000000;
    wire _637;
    wire _636;
    wire _635;
    wire _634;
    wire _633;
    wire _632;
    wire _631;
    wire _630;
    wire _629;
    wire [8:0] _638;
    wire [8:0] address_28;
    wire write_enable_28 = 1'b0;
    wire _626;
    wire read_enable_22;
    wire [63:0] data_25;
    wire [63:0] data_26;
    wire _623;
    wire _622;
    wire _621;
    wire _620;
    wire _619;
    wire _618;
    wire _617;
    wire _616;
    wire _615;
    wire [8:0] _624;
    wire [8:0] address_29;
    wire _612;
    wire read_enable_23;
    wire _610;
    wire write_enable_29;
    wire _614;
    wire [131:0] _643;
    wire [63:0] _644;
    wire _569 = 1'b0;
    wire _568 = 1'b0;
    wire _571;
    wire _19;
    reg PHASE_5;
    wire [63:0] _658;
    wire [8:0] address_30;
    wire _602;
    wire read_enable_24;
    wire write_enable_30;
    wire _604;
    wire [8:0] address_31;
    wire _597;
    wire read_enable_25;
    wire _595;
    wire write_enable_31;
    wire _599;
    wire [131:0] _607;
    wire [63:0] _608;
    wire [63:0] _669;
    wire [63:0] data_27;
    wire [63:0] data_28;
    wire [63:0] data_29;
    wire [63:0] data_30;
    wire [8:0] address_32;
    wire _588;
    wire read_enable_26;
    wire _585;
    wire _586;
    wire write_enable_32;
    wire _590;
    wire [8:0] address_33;
    wire _581;
    wire read_enable_27;
    wire _578;
    wire _579;
    wire write_enable_33;
    wire _583;
    wire [131:0] _593;
    wire [63:0] _594;
    wire _576 = 1'b0;
    wire _575 = 1'b0;
    wire _670;
    wire _21;
    reg PHASE_6;
    wire [63:0] q0_1;
    wire _573 = 1'b0;
    wire _572 = 1'b0;
    reg _574;
    wire [63:0] _659;
    wire [191:0] _668;
    wire [63:0] _671;
    wire [63:0] data_31;
    wire [63:0] data_32;
    wire [63:0] data_33;
    wire [63:0] data_34;
    wire [8:0] address_34;
    wire _735;
    wire _734;
    wire read_enable_28;
    wire _728 = 1'b0;
    wire _727 = 1'b0;
    wire _725 = 1'b0;
    wire _724 = 1'b0;
    wire _722 = 1'b0;
    wire _721 = 1'b0;
    wire _719 = 1'b0;
    wire _718 = 1'b0;
    wire _716 = 1'b0;
    wire _715 = 1'b0;
    wire _713 = 1'b0;
    wire _712 = 1'b0;
    wire _710 = 1'b0;
    wire _709 = 1'b0;
    wire _707 = 1'b0;
    wire _706 = 1'b0;
    wire _704 = 1'b0;
    wire _703 = 1'b0;
    reg _705;
    reg _708;
    reg _711;
    reg _714;
    reg _717;
    reg _720;
    reg _723;
    reg _726;
    reg _729;
    wire _730;
    wire _701 = 1'b0;
    wire _700 = 1'b0;
    wire _698 = 1'b0;
    wire _697 = 1'b0;
    wire _695 = 1'b0;
    wire _694 = 1'b0;
    wire _692 = 1'b0;
    wire _691 = 1'b0;
    wire _689 = 1'b0;
    wire _688 = 1'b0;
    wire _686 = 1'b0;
    wire _685 = 1'b0;
    wire _683 = 1'b0;
    wire _682 = 1'b0;
    wire _680 = 1'b0;
    wire _679 = 1'b0;
    wire _677 = 1'b0;
    wire _676 = 1'b0;
    reg _678;
    reg _681;
    reg _684;
    reg _687;
    reg _690;
    reg _693;
    reg _696;
    reg _699;
    reg _702;
    wire _731;
    wire _732;
    wire write_enable_34;
    wire _737;
    wire [131:0] _744;
    wire [63:0] _745;
    wire _673 = 1'b0;
    wire _672 = 1'b0;
    wire _675;
    wire _23;
    reg PHASE_7;
    wire [63:0] _757;
    wire [8:0] address_35;
    wire write_enable_35;
    wire [8:0] address_36;
    wire _938;
    wire read_enable_29;
    wire _936;
    wire write_enable_36;
    wire _940;
    wire [131:0] _945;
    wire [63:0] _946;
    wire [8:0] _931 = 9'b000000000;
    wire [8:0] address_37;
    wire _929;
    wire write_enable_37;
    wire [63:0] _854;
    wire [63:0] _853;
    wire [63:0] _855;
    wire [63:0] _851;
    wire [63:0] _850;
    wire [63:0] q1_2;
    wire [63:0] _856;
    wire [8:0] address_38;
    wire write_enable_38 = 1'b0;
    wire _841;
    wire read_enable_30;
    wire [8:0] address_39;
    wire _837;
    wire read_enable_31;
    wire _835;
    wire write_enable_39;
    wire _839;
    wire [131:0] _846;
    wire [63:0] _847;
    wire [63:0] data_35 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_36;
    wire [8:0] _829 = 9'b000000000;
    wire _827;
    wire _826;
    wire _825;
    wire _824;
    wire _823;
    wire _822;
    wire _821;
    wire _820;
    wire _819;
    wire [8:0] _828;
    wire [8:0] address_40;
    wire write_enable_40 = 1'b0;
    wire _816;
    wire read_enable_32;
    wire [63:0] data_37;
    wire [63:0] data_38;
    wire _813;
    wire _812;
    wire _811;
    wire _810;
    wire _809;
    wire _808;
    wire _807;
    wire _806;
    wire _805;
    wire [8:0] _814;
    wire [8:0] address_41;
    wire _802;
    wire read_enable_33;
    wire _800;
    wire write_enable_41;
    wire _804;
    wire [131:0] _833;
    wire [63:0] _834;
    wire _759 = 1'b0;
    wire _758 = 1'b0;
    wire _761;
    wire _27;
    reg PHASE_8;
    wire [63:0] _848;
    wire [8:0] address_42;
    wire _792;
    wire read_enable_34;
    wire write_enable_42;
    wire _794;
    wire [8:0] address_43;
    wire _787;
    wire read_enable_35;
    wire _785;
    wire write_enable_43;
    wire _789;
    wire [131:0] _797;
    wire [63:0] _798;
    wire [63:0] _859;
    wire [63:0] data_39;
    wire [63:0] data_40;
    wire [63:0] data_41;
    wire [63:0] data_42;
    wire [8:0] address_44;
    wire _778;
    wire read_enable_36;
    wire _775;
    wire _776;
    wire write_enable_44;
    wire _780;
    wire [8:0] address_45;
    wire _771;
    wire read_enable_37;
    wire _768;
    wire _769;
    wire write_enable_45;
    wire _773;
    wire [131:0] _783;
    wire [63:0] _784;
    wire _766 = 1'b0;
    wire _765 = 1'b0;
    wire _860;
    wire _29;
    reg PHASE_9;
    wire [63:0] q0_2;
    wire _763 = 1'b0;
    wire _762 = 1'b0;
    reg _764;
    wire [63:0] _849;
    wire [191:0] _858;
    wire [63:0] _861;
    wire [63:0] data_43;
    wire [63:0] data_44;
    wire [63:0] data_45;
    wire [63:0] data_46;
    wire [8:0] address_46;
    wire _925;
    wire _924;
    wire read_enable_38;
    wire _918 = 1'b0;
    wire _917 = 1'b0;
    wire _915 = 1'b0;
    wire _914 = 1'b0;
    wire _912 = 1'b0;
    wire _911 = 1'b0;
    wire _909 = 1'b0;
    wire _908 = 1'b0;
    wire _906 = 1'b0;
    wire _905 = 1'b0;
    wire _903 = 1'b0;
    wire _902 = 1'b0;
    wire _900 = 1'b0;
    wire _899 = 1'b0;
    wire _897 = 1'b0;
    wire _896 = 1'b0;
    wire _894 = 1'b0;
    wire _893 = 1'b0;
    reg _895;
    reg _898;
    reg _901;
    reg _904;
    reg _907;
    reg _910;
    reg _913;
    reg _916;
    reg _919;
    wire _920;
    wire _891 = 1'b0;
    wire _890 = 1'b0;
    wire _888 = 1'b0;
    wire _887 = 1'b0;
    wire _885 = 1'b0;
    wire _884 = 1'b0;
    wire _882 = 1'b0;
    wire _881 = 1'b0;
    wire _879 = 1'b0;
    wire _878 = 1'b0;
    wire _876 = 1'b0;
    wire _875 = 1'b0;
    wire _873 = 1'b0;
    wire _872 = 1'b0;
    wire _870 = 1'b0;
    wire _869 = 1'b0;
    wire _867 = 1'b0;
    wire _866 = 1'b0;
    reg _868;
    reg _871;
    reg _874;
    reg _877;
    reg _880;
    reg _883;
    reg _886;
    reg _889;
    reg _892;
    wire _921;
    wire _922;
    wire write_enable_46;
    wire _927;
    wire [131:0] _934;
    wire [63:0] _935;
    wire _863 = 1'b0;
    wire _862 = 1'b0;
    wire _865;
    wire _31;
    reg PHASE_10;
    wire [63:0] _947;
    wire [8:0] address_47;
    wire write_enable_47;
    wire [8:0] address_48;
    wire _1128;
    wire read_enable_39;
    wire _1126;
    wire write_enable_48;
    wire _1130;
    wire [131:0] _1135;
    wire [63:0] _1136;
    wire [8:0] _1121 = 9'b000000000;
    wire [8:0] address_49;
    wire _1119;
    wire write_enable_49;
    wire [63:0] _1044;
    wire [63:0] _1043;
    wire [63:0] _1045;
    wire [63:0] _1041;
    wire [63:0] _1040;
    wire [63:0] q1_3;
    wire [63:0] _1046;
    wire [8:0] address_50;
    wire write_enable_50 = 1'b0;
    wire _1031;
    wire read_enable_40;
    wire [8:0] address_51;
    wire _1027;
    wire read_enable_41;
    wire _1025;
    wire write_enable_51;
    wire _1029;
    wire [131:0] _1036;
    wire [63:0] _1037;
    wire [63:0] data_47 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_48;
    wire [8:0] _1019 = 9'b000000000;
    wire _1017;
    wire _1016;
    wire _1015;
    wire _1014;
    wire _1013;
    wire _1012;
    wire _1011;
    wire _1010;
    wire _1009;
    wire [8:0] _1018;
    wire [8:0] address_52;
    wire write_enable_52 = 1'b0;
    wire _1006;
    wire read_enable_42;
    wire [63:0] data_49;
    wire [63:0] data_50;
    wire _1003;
    wire _1002;
    wire _1001;
    wire _1000;
    wire _999;
    wire _998;
    wire _997;
    wire _996;
    wire _995;
    wire [8:0] _1004;
    wire [8:0] address_53;
    wire _992;
    wire read_enable_43;
    wire _990;
    wire write_enable_53;
    wire _994;
    wire [131:0] _1023;
    wire [63:0] _1024;
    wire _949 = 1'b0;
    wire _948 = 1'b0;
    wire _951;
    wire _35;
    reg PHASE_11;
    wire [63:0] _1038;
    wire [8:0] address_54;
    wire _982;
    wire read_enable_44;
    wire write_enable_54;
    wire _984;
    wire [8:0] address_55;
    wire _977;
    wire read_enable_45;
    wire _975;
    wire write_enable_55;
    wire _979;
    wire [131:0] _987;
    wire [63:0] _988;
    wire [63:0] _1049;
    wire [63:0] data_51;
    wire [63:0] data_52;
    wire [63:0] data_53;
    wire [63:0] data_54;
    wire [8:0] address_56;
    wire _968;
    wire read_enable_46;
    wire _965;
    wire _966;
    wire write_enable_56;
    wire _970;
    wire [8:0] address_57;
    wire _961;
    wire read_enable_47;
    wire _958;
    wire _959;
    wire write_enable_57;
    wire _963;
    wire [131:0] _973;
    wire [63:0] _974;
    wire _956 = 1'b0;
    wire _955 = 1'b0;
    wire _1050;
    wire _37;
    reg PHASE_12;
    wire [63:0] q0_3;
    wire _953 = 1'b0;
    wire _952 = 1'b0;
    reg _954;
    wire [63:0] _1039;
    wire [191:0] _1048;
    wire [63:0] _1051;
    wire [63:0] data_55;
    wire [63:0] data_56;
    wire [63:0] data_57;
    wire [63:0] data_58;
    wire [8:0] address_58;
    wire _1115;
    wire _1114;
    wire read_enable_48;
    wire _1108 = 1'b0;
    wire _1107 = 1'b0;
    wire _1105 = 1'b0;
    wire _1104 = 1'b0;
    wire _1102 = 1'b0;
    wire _1101 = 1'b0;
    wire _1099 = 1'b0;
    wire _1098 = 1'b0;
    wire _1096 = 1'b0;
    wire _1095 = 1'b0;
    wire _1093 = 1'b0;
    wire _1092 = 1'b0;
    wire _1090 = 1'b0;
    wire _1089 = 1'b0;
    wire _1087 = 1'b0;
    wire _1086 = 1'b0;
    wire _1084 = 1'b0;
    wire _1083 = 1'b0;
    reg _1085;
    reg _1088;
    reg _1091;
    reg _1094;
    reg _1097;
    reg _1100;
    reg _1103;
    reg _1106;
    reg _1109;
    wire _1110;
    wire _1081 = 1'b0;
    wire _1080 = 1'b0;
    wire _1078 = 1'b0;
    wire _1077 = 1'b0;
    wire _1075 = 1'b0;
    wire _1074 = 1'b0;
    wire _1072 = 1'b0;
    wire _1071 = 1'b0;
    wire _1069 = 1'b0;
    wire _1068 = 1'b0;
    wire _1066 = 1'b0;
    wire _1065 = 1'b0;
    wire _1063 = 1'b0;
    wire _1062 = 1'b0;
    wire _1060 = 1'b0;
    wire _1059 = 1'b0;
    wire _1057 = 1'b0;
    wire _1056 = 1'b0;
    reg _1058;
    reg _1061;
    reg _1064;
    reg _1067;
    reg _1070;
    reg _1073;
    reg _1076;
    reg _1079;
    reg _1082;
    wire _1111;
    wire _1112;
    wire write_enable_58;
    wire _1117;
    wire [131:0] _1124;
    wire [63:0] _1125;
    wire _1053 = 1'b0;
    wire _1052 = 1'b0;
    wire _1055;
    wire _39;
    reg PHASE_13;
    wire [63:0] _1137;
    wire [8:0] address_59;
    wire write_enable_59;
    wire [8:0] address_60;
    wire _1318;
    wire read_enable_49;
    wire _1316;
    wire write_enable_60;
    wire _1320;
    wire [131:0] _1325;
    wire [63:0] _1326;
    wire [8:0] _1311 = 9'b000000000;
    wire [8:0] address_61;
    wire _1309;
    wire write_enable_61;
    wire [63:0] _1234;
    wire [63:0] _1233;
    wire [63:0] _1235;
    wire [63:0] _1231;
    wire [63:0] _1230;
    wire [63:0] q1_4;
    wire [63:0] _1236;
    wire [8:0] address_62;
    wire write_enable_62 = 1'b0;
    wire _1221;
    wire read_enable_50;
    wire [8:0] address_63;
    wire _1217;
    wire read_enable_51;
    wire _1215;
    wire write_enable_63;
    wire _1219;
    wire [131:0] _1226;
    wire [63:0] _1227;
    wire [63:0] data_59 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_60;
    wire [8:0] _1209 = 9'b000000000;
    wire _1207;
    wire _1206;
    wire _1205;
    wire _1204;
    wire _1203;
    wire _1202;
    wire _1201;
    wire _1200;
    wire _1199;
    wire [8:0] _1208;
    wire [8:0] address_64;
    wire write_enable_64 = 1'b0;
    wire _1196;
    wire read_enable_52;
    wire [63:0] data_61;
    wire [63:0] data_62;
    wire _1193;
    wire _1192;
    wire _1191;
    wire _1190;
    wire _1189;
    wire _1188;
    wire _1187;
    wire _1186;
    wire _1185;
    wire [8:0] _1194;
    wire [8:0] address_65;
    wire _1182;
    wire read_enable_53;
    wire _1180;
    wire write_enable_65;
    wire _1184;
    wire [131:0] _1213;
    wire [63:0] _1214;
    wire _1139 = 1'b0;
    wire _1138 = 1'b0;
    wire _1141;
    wire _43;
    reg PHASE_14;
    wire [63:0] _1228;
    wire [8:0] address_66;
    wire _1172;
    wire read_enable_54;
    wire write_enable_66;
    wire _1174;
    wire [8:0] address_67;
    wire _1167;
    wire read_enable_55;
    wire _1165;
    wire write_enable_67;
    wire _1169;
    wire [131:0] _1177;
    wire [63:0] _1178;
    wire [63:0] _1239;
    wire [63:0] data_63;
    wire [63:0] data_64;
    wire [63:0] data_65;
    wire [63:0] data_66;
    wire [8:0] address_68;
    wire _1158;
    wire read_enable_56;
    wire _1155;
    wire _1156;
    wire write_enable_68;
    wire _1160;
    wire [8:0] address_69;
    wire _1151;
    wire read_enable_57;
    wire _1148;
    wire _1149;
    wire write_enable_69;
    wire _1153;
    wire [131:0] _1163;
    wire [63:0] _1164;
    wire _1146 = 1'b0;
    wire _1145 = 1'b0;
    wire _1240;
    wire _45;
    reg PHASE_15;
    wire [63:0] q0_4;
    wire _1143 = 1'b0;
    wire _1142 = 1'b0;
    reg _1144;
    wire [63:0] _1229;
    wire [191:0] _1238;
    wire [63:0] _1241;
    wire [63:0] data_67;
    wire [63:0] data_68;
    wire [63:0] data_69;
    wire [63:0] data_70;
    wire [8:0] address_70;
    wire _1305;
    wire _1304;
    wire read_enable_58;
    wire _1298 = 1'b0;
    wire _1297 = 1'b0;
    wire _1295 = 1'b0;
    wire _1294 = 1'b0;
    wire _1292 = 1'b0;
    wire _1291 = 1'b0;
    wire _1289 = 1'b0;
    wire _1288 = 1'b0;
    wire _1286 = 1'b0;
    wire _1285 = 1'b0;
    wire _1283 = 1'b0;
    wire _1282 = 1'b0;
    wire _1280 = 1'b0;
    wire _1279 = 1'b0;
    wire _1277 = 1'b0;
    wire _1276 = 1'b0;
    wire _1274 = 1'b0;
    wire _1273 = 1'b0;
    reg _1275;
    reg _1278;
    reg _1281;
    reg _1284;
    reg _1287;
    reg _1290;
    reg _1293;
    reg _1296;
    reg _1299;
    wire _1300;
    wire _1271 = 1'b0;
    wire _1270 = 1'b0;
    wire _1268 = 1'b0;
    wire _1267 = 1'b0;
    wire _1265 = 1'b0;
    wire _1264 = 1'b0;
    wire _1262 = 1'b0;
    wire _1261 = 1'b0;
    wire _1259 = 1'b0;
    wire _1258 = 1'b0;
    wire _1256 = 1'b0;
    wire _1255 = 1'b0;
    wire _1253 = 1'b0;
    wire _1252 = 1'b0;
    wire _1250 = 1'b0;
    wire _1249 = 1'b0;
    wire _1247 = 1'b0;
    wire _1246 = 1'b0;
    reg _1248;
    reg _1251;
    reg _1254;
    reg _1257;
    reg _1260;
    reg _1263;
    reg _1266;
    reg _1269;
    reg _1272;
    wire _1301;
    wire _1302;
    wire write_enable_70;
    wire _1307;
    wire [131:0] _1314;
    wire [63:0] _1315;
    wire _1243 = 1'b0;
    wire _1242 = 1'b0;
    wire _1245;
    wire _47;
    reg PHASE_16;
    wire [63:0] _1327;
    wire [8:0] address_71;
    wire write_enable_71;
    wire [8:0] address_72;
    wire _1508;
    wire read_enable_59;
    wire _1506;
    wire write_enable_72;
    wire _1510;
    wire [131:0] _1515;
    wire [63:0] _1516;
    wire [8:0] _1501 = 9'b000000000;
    wire [8:0] address_73;
    wire _1499;
    wire write_enable_73;
    wire [63:0] _1424;
    wire [63:0] _1423;
    wire [63:0] _1425;
    wire [63:0] _1421;
    wire [63:0] _1420;
    wire [63:0] q1_5;
    wire [63:0] _1426;
    wire [8:0] address_74;
    wire write_enable_74 = 1'b0;
    wire _1411;
    wire read_enable_60;
    wire [8:0] address_75;
    wire _1407;
    wire read_enable_61;
    wire _1405;
    wire write_enable_75;
    wire _1409;
    wire [131:0] _1416;
    wire [63:0] _1417;
    wire [63:0] data_71 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_72;
    wire [8:0] _1399 = 9'b000000000;
    wire _1397;
    wire _1396;
    wire _1395;
    wire _1394;
    wire _1393;
    wire _1392;
    wire _1391;
    wire _1390;
    wire _1389;
    wire [8:0] _1398;
    wire [8:0] address_76;
    wire write_enable_76 = 1'b0;
    wire _1386;
    wire read_enable_62;
    wire [63:0] data_73;
    wire [63:0] data_74;
    wire _1383;
    wire _1382;
    wire _1381;
    wire _1380;
    wire _1379;
    wire _1378;
    wire _1377;
    wire _1376;
    wire _1375;
    wire [8:0] _1384;
    wire [8:0] address_77;
    wire _1372;
    wire read_enable_63;
    wire _1370;
    wire write_enable_77;
    wire _1374;
    wire [131:0] _1403;
    wire [63:0] _1404;
    wire _1329 = 1'b0;
    wire _1328 = 1'b0;
    wire _1331;
    wire _51;
    reg PHASE_17;
    wire [63:0] _1418;
    wire [8:0] address_78;
    wire _1362;
    wire read_enable_64;
    wire write_enable_78;
    wire _1364;
    wire [8:0] address_79;
    wire _1357;
    wire read_enable_65;
    wire _1355;
    wire write_enable_79;
    wire _1359;
    wire [131:0] _1367;
    wire [63:0] _1368;
    wire [63:0] _1429;
    wire [63:0] data_75;
    wire [63:0] data_76;
    wire [63:0] data_77;
    wire [63:0] data_78;
    wire [8:0] address_80;
    wire _1348;
    wire read_enable_66;
    wire _1345;
    wire _1346;
    wire write_enable_80;
    wire _1350;
    wire [8:0] address_81;
    wire _1341;
    wire read_enable_67;
    wire _1338;
    wire _1339;
    wire write_enable_81;
    wire _1343;
    wire [131:0] _1353;
    wire [63:0] _1354;
    wire _1336 = 1'b0;
    wire _1335 = 1'b0;
    wire _1430;
    wire _53;
    reg PHASE_18;
    wire [63:0] q0_5;
    wire _1333 = 1'b0;
    wire _1332 = 1'b0;
    reg _1334;
    wire [63:0] _1419;
    wire [191:0] _1428;
    wire [63:0] _1431;
    wire [63:0] data_79;
    wire [63:0] data_80;
    wire [63:0] data_81;
    wire [63:0] data_82;
    wire [8:0] address_82;
    wire _1495;
    wire _1494;
    wire read_enable_68;
    wire _1488 = 1'b0;
    wire _1487 = 1'b0;
    wire _1485 = 1'b0;
    wire _1484 = 1'b0;
    wire _1482 = 1'b0;
    wire _1481 = 1'b0;
    wire _1479 = 1'b0;
    wire _1478 = 1'b0;
    wire _1476 = 1'b0;
    wire _1475 = 1'b0;
    wire _1473 = 1'b0;
    wire _1472 = 1'b0;
    wire _1470 = 1'b0;
    wire _1469 = 1'b0;
    wire _1467 = 1'b0;
    wire _1466 = 1'b0;
    wire _1464 = 1'b0;
    wire _1463 = 1'b0;
    reg _1465;
    reg _1468;
    reg _1471;
    reg _1474;
    reg _1477;
    reg _1480;
    reg _1483;
    reg _1486;
    reg _1489;
    wire _1490;
    wire _1461 = 1'b0;
    wire _1460 = 1'b0;
    wire _1458 = 1'b0;
    wire _1457 = 1'b0;
    wire _1455 = 1'b0;
    wire _1454 = 1'b0;
    wire _1452 = 1'b0;
    wire _1451 = 1'b0;
    wire _1449 = 1'b0;
    wire _1448 = 1'b0;
    wire _1446 = 1'b0;
    wire _1445 = 1'b0;
    wire _1443 = 1'b0;
    wire _1442 = 1'b0;
    wire _1440 = 1'b0;
    wire _1439 = 1'b0;
    wire _1437 = 1'b0;
    wire _1436 = 1'b0;
    reg _1438;
    reg _1441;
    reg _1444;
    reg _1447;
    reg _1450;
    reg _1453;
    reg _1456;
    reg _1459;
    reg _1462;
    wire _1491;
    wire _1492;
    wire write_enable_82;
    wire _1497;
    wire [131:0] _1504;
    wire [63:0] _1505;
    wire _1433 = 1'b0;
    wire _1432 = 1'b0;
    wire _1435;
    wire _55;
    reg PHASE_19;
    wire [63:0] _1517;
    wire [8:0] address_83;
    wire write_enable_83;
    wire [8:0] address_84;
    wire _1698;
    wire read_enable_69;
    wire _1696;
    wire write_enable_84;
    wire _1700;
    wire [131:0] _1705;
    wire [63:0] _1706;
    wire [8:0] _1691 = 9'b000000000;
    wire [8:0] address_85;
    wire _1689;
    wire write_enable_85;
    wire [3:0] _286;
    wire _285;
    wire _283;
    wire [63:0] _282;
    wire [63:0] _281;
    wire [63:0] _280;
    wire [63:0] _279;
    wire [63:0] _278;
    wire [63:0] _277;
    wire [63:0] _276;
    wire [63:0] _1614;
    wire [63:0] _1613;
    wire [63:0] _1615;
    wire [63:0] _1611;
    wire [63:0] _1610;
    wire [63:0] q1_6;
    wire [63:0] _1616;
    wire [8:0] address_86;
    wire write_enable_86 = 1'b0;
    wire _1601;
    wire read_enable_70;
    wire [8:0] address_87;
    wire _1597;
    wire read_enable_71;
    wire _1595;
    wire write_enable_87;
    wire _1599;
    wire [131:0] _1606;
    wire [63:0] _1607;
    wire [63:0] data_83 = 64'b0000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] data_84;
    wire [8:0] _1589 = 9'b000000000;
    wire _1587;
    wire _1586;
    wire _1585;
    wire _1584;
    wire _1583;
    wire _1582;
    wire _1581;
    wire _1580;
    wire _1579;
    wire [8:0] _1588;
    wire [8:0] address_88;
    wire write_enable_88 = 1'b0;
    wire _1576;
    wire read_enable_72;
    wire [63:0] data_85;
    wire [63:0] data_86;
    wire [8:0] _60;
    wire _1573;
    wire _1572;
    wire _1571;
    wire _1570;
    wire _1569;
    wire _1568;
    wire _1567;
    wire _1566;
    wire _1565;
    wire [8:0] _1574;
    wire [8:0] address_89;
    wire _1562;
    wire read_enable_73;
    wire [7:0] _62;
    wire _1560;
    wire write_enable_89;
    wire _1564;
    wire [131:0] _1593;
    wire [63:0] _1594;
    wire _1519 = 1'b0;
    wire _1518 = 1'b0;
    wire _1521;
    wire _63;
    reg PHASE_20;
    wire [63:0] _1608;
    wire [8:0] address_90;
    wire _1552;
    wire read_enable_74;
    wire write_enable_90;
    wire _1554;
    wire [8:0] address_91;
    wire _1547;
    wire read_enable_75;
    wire _1545;
    wire write_enable_91;
    wire _1549;
    wire [131:0] _1557;
    wire [63:0] _1558;
    wire [63:0] _1619;
    wire [63:0] data_87;
    wire [63:0] data_88;
    wire [63:0] data_89;
    wire [63:0] data_90;
    wire [8:0] _198 = 9'b000000000;
    wire [8:0] _197 = 9'b000000000;
    wire [8:0] _195 = 9'b000000000;
    wire [8:0] _194 = 9'b000000000;
    wire [8:0] _192 = 9'b000000000;
    wire [8:0] _191 = 9'b000000000;
    wire [8:0] _189 = 9'b000000000;
    wire [8:0] _188 = 9'b000000000;
    wire [8:0] _186 = 9'b000000000;
    wire [8:0] _185 = 9'b000000000;
    wire [8:0] _183 = 9'b000000000;
    wire [8:0] _182 = 9'b000000000;
    wire [8:0] _180 = 9'b000000000;
    wire [8:0] _179 = 9'b000000000;
    wire [8:0] _177 = 9'b000000000;
    wire [8:0] _176 = 9'b000000000;
    wire [8:0] _174 = 9'b000000000;
    wire [8:0] _173 = 9'b000000000;
    reg [8:0] _175;
    reg [8:0] _178;
    reg [8:0] _181;
    reg [8:0] _184;
    reg [8:0] _187;
    reg [8:0] _190;
    reg [8:0] _193;
    reg [8:0] _196;
    reg [8:0] _199;
    wire [8:0] _172;
    wire [8:0] address_92;
    wire _1538;
    wire read_enable_76;
    wire _1535;
    wire _1536;
    wire write_enable_92;
    wire _1540;
    wire [8:0] address_93;
    wire _1531;
    wire read_enable_77;
    wire _1528;
    wire _1529;
    wire write_enable_93;
    wire _1533;
    wire [131:0] _1543;
    wire [63:0] _1544;
    wire _99;
    wire _1526 = 1'b0;
    wire _1525 = 1'b0;
    wire _1620;
    wire _65;
    reg PHASE_21;
    wire [63:0] q0_6;
    wire _1523 = 1'b0;
    wire _1522 = 1'b0;
    wire _92;
    reg _1524;
    wire [63:0] _1609;
    wire [191:0] _1618;
    wire [63:0] _1621;
    wire [63:0] data_91;
    wire [63:0] data_92;
    wire [63:0] data_93;
    wire [63:0] data_94;
    wire [8:0] _163 = 9'b000000000;
    wire [8:0] _162 = 9'b000000000;
    wire [8:0] _160 = 9'b000000000;
    wire [8:0] _159 = 9'b000000000;
    wire [8:0] _157 = 9'b000000000;
    wire [8:0] _156 = 9'b000000000;
    wire [8:0] _154 = 9'b000000000;
    wire [8:0] _153 = 9'b000000000;
    wire [8:0] _151 = 9'b000000000;
    wire [8:0] _150 = 9'b000000000;
    wire [8:0] _148 = 9'b000000000;
    wire [8:0] _147 = 9'b000000000;
    wire [8:0] _145 = 9'b000000000;
    wire [8:0] _144 = 9'b000000000;
    wire [8:0] _142 = 9'b000000000;
    wire [8:0] _141 = 9'b000000000;
    wire [8:0] _139 = 9'b000000000;
    wire [8:0] _138 = 9'b000000000;
    wire [8:0] _137;
    reg [8:0] _140;
    reg [8:0] _143;
    reg [8:0] _146;
    reg [8:0] _149;
    reg [8:0] _152;
    reg [8:0] _155;
    reg [8:0] _158;
    reg [8:0] _161;
    reg [8:0] _164;
    wire [8:0] _68;
    wire [8:0] address_94;
    wire _1685;
    wire [7:0] _70;
    wire _1684;
    wire read_enable_78;
    wire _1678 = 1'b0;
    wire _1677 = 1'b0;
    wire _1675 = 1'b0;
    wire _1674 = 1'b0;
    wire _1672 = 1'b0;
    wire _1671 = 1'b0;
    wire _1669 = 1'b0;
    wire _1668 = 1'b0;
    wire _1666 = 1'b0;
    wire _1665 = 1'b0;
    wire _1663 = 1'b0;
    wire _1662 = 1'b0;
    wire _1660 = 1'b0;
    wire _1659 = 1'b0;
    wire _1657 = 1'b0;
    wire _1656 = 1'b0;
    wire _1654 = 1'b0;
    wire _1653 = 1'b0;
    wire _284;
    reg _1655;
    reg _1658;
    reg _1661;
    reg _1664;
    reg _1667;
    reg _1670;
    reg _1673;
    reg _1676;
    reg _1679;
    wire _1680;
    wire _1651 = 1'b0;
    wire _1650 = 1'b0;
    wire _1648 = 1'b0;
    wire _1647 = 1'b0;
    wire _1645 = 1'b0;
    wire _1644 = 1'b0;
    wire _1642 = 1'b0;
    wire _1641 = 1'b0;
    wire _1639 = 1'b0;
    wire _1638 = 1'b0;
    wire _1636 = 1'b0;
    wire _1635 = 1'b0;
    wire _1633 = 1'b0;
    wire _1632 = 1'b0;
    wire _1630 = 1'b0;
    wire _1629 = 1'b0;
    wire _1627 = 1'b0;
    wire _1626 = 1'b0;
    wire _130;
    reg _1628;
    reg _1631;
    reg _1634;
    reg _1637;
    reg _1640;
    reg _1643;
    reg _1646;
    reg _1649;
    reg _1652;
    wire _1681;
    wire _128 = 1'b0;
    wire _127 = 1'b0;
    wire _125 = 1'b0;
    wire _124 = 1'b0;
    wire _122 = 1'b0;
    wire _121 = 1'b0;
    wire _119 = 1'b0;
    wire _118 = 1'b0;
    wire _116 = 1'b0;
    wire _115 = 1'b0;
    wire _113 = 1'b0;
    wire _112 = 1'b0;
    wire _110 = 1'b0;
    wire _109 = 1'b0;
    wire _107 = 1'b0;
    wire _106 = 1'b0;
    wire vdd = 1'b1;
    wire _104 = 1'b0;
    wire _103 = 1'b0;
    wire _102;
    reg _105;
    reg _108;
    reg _111;
    reg _114;
    reg _117;
    reg _120;
    reg _123;
    reg _126;
    reg _129;
    wire _1682;
    wire write_enable_94;
    wire _1687;
    wire gnd = 1'b0;
    wire [131:0] _1694;
    wire [63:0] _1695;
    wire _72;
    wire _1623 = 1'b0;
    wire _1622 = 1'b0;
    wire _1625;
    wire _73;
    reg PHASE_22;
    wire [63:0] _1707;
    wire _76;
    wire _78;
    wire _80;
    wire _82;
    wire _84;
    wire [508:0] _91;
    wire _1708;

    /* logic */
    assign address = _366 ? _199 : _361;
    assign write_enable = _359 & _366;
    assign address_0 = _366 ? _164 : _68;
    assign _368 = ~ _366;
    assign read_enable = _354 & _368;
    assign _366 = ~ PHASE_1;
    assign write_enable_0 = _352 & _366;
    assign _370 = write_enable_0 | read_enable;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_370), .regcea(vdd), .wea(write_enable_0), .addra(address_0), .dina(data_7), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable), .regceb(vdd), .web(write_enable), .addrb(address), .dinb(data_3), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_375[131:131]), .sbiterrb(_375[130:130]), .doutb(_375[129:66]), .dbiterra(_375[65:65]), .sbiterra(_375[64:64]), .douta(_375[63:0]) );
    assign _376 = _375[63:0];
    assign address_1 = PHASE_1 ? _199 : _361;
    assign _359 = _129 & _322;
    assign write_enable_1 = _359 & PHASE_1;
    assign _273 = _265[129:66];
    assign _272 = _252[129:66];
    assign _274 = PHASE ? _273 : _272;
    assign _270 = _216[129:66];
    assign _269 = _202[129:66];
    assign q1 = PHASE_0 ? _270 : _269;
    assign _275 = _96 ? _274 : q1;
    assign address_2 = _254 ? _248 : _247;
    assign _260 = ~ _254;
    assign read_enable_0 = _102 & _260;
    assign address_3 = _254 ? _60 : _233;
    assign _256 = ~ _254;
    assign read_enable_1 = _102 & _256;
    assign _254 = ~ PHASE;
    assign write_enable_3 = _219 & _254;
    assign _258 = write_enable_3 | read_enable_1;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_0
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_258), .regcea(vdd), .wea(write_enable_3), .addra(address_3), .dina(data_1), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_0), .regceb(vdd), .web(write_enable_2), .addrb(address_2), .dinb(data), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_265[131:131]), .sbiterrb(_265[130:130]), .doutb(_265[129:66]), .dbiterra(_265[65:65]), .sbiterra(_265[64:64]), .douta(_265[63:0]) );
    assign _266 = _265[63:0];
    assign _246 = _172[8:8];
    assign _245 = _172[7:7];
    assign _244 = _172[6:6];
    assign _243 = _172[5:5];
    assign _242 = _172[4:4];
    assign _241 = _172[3:3];
    assign _240 = _172[2:2];
    assign _239 = _172[1:1];
    assign _238 = _172[0:0];
    assign _247 = { _238, _239, _240, _241, _242, _243, _244, _245, _246 };
    assign address_4 = PHASE ? _248 : _247;
    assign _235 = ~ PHASE;
    assign read_enable_2 = _102 & _235;
    assign data_1 = wr_d7;
    assign _232 = _137[8:8];
    assign _231 = _137[7:7];
    assign _230 = _137[6:6];
    assign _229 = _137[5:5];
    assign _228 = _137[4:4];
    assign _227 = _137[3:3];
    assign _226 = _137[2:2];
    assign _225 = _137[1:1];
    assign _224 = _137[0:0];
    assign _233 = { _224, _225, _226, _227, _228, _229, _230, _231, _232 };
    assign address_5 = PHASE ? _60 : _233;
    assign _221 = ~ PHASE;
    assign read_enable_3 = _102 & _221;
    assign _219 = _62[7:7];
    assign write_enable_5 = _219 & PHASE;
    assign _223 = write_enable_5 | read_enable_3;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_1
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_223), .regcea(vdd), .wea(write_enable_5), .addra(address_5), .dina(data_1), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_2), .regceb(vdd), .web(write_enable_4), .addrb(address_4), .dinb(data), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_252[131:131]), .sbiterrb(_252[130:130]), .doutb(_252[129:66]), .dbiterra(_252[65:65]), .sbiterra(_252[64:64]), .douta(_252[63:0]) );
    assign _253 = _252[63:0];
    assign _89 = ~ PHASE;
    assign _3 = _89;
    always @(posedge _84) begin
        if (_82)
            PHASE <= _87;
        else
            if (_72)
                PHASE <= _3;
    end
    assign _267 = PHASE ? _266 : _253;
    assign address_6 = _204 ? _199 : _172;
    assign _211 = ~ _204;
    assign read_enable_4 = _102 & _211;
    assign write_enable_6 = _167 & _204;
    assign _213 = write_enable_6 | read_enable_4;
    assign address_7 = _204 ? _164 : _137;
    assign _206 = ~ _204;
    assign read_enable_5 = _102 & _206;
    assign _204 = ~ PHASE_0;
    assign write_enable_7 = _132 & _204;
    assign _208 = write_enable_7 | read_enable_5;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_2
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_208), .regcea(vdd), .wea(write_enable_7), .addra(address_7), .dina(data_7), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_213), .regceb(vdd), .web(write_enable_6), .addrb(address_6), .dinb(data_3), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_216[131:131]), .sbiterrb(_216[130:130]), .doutb(_216[129:66]), .dbiterra(_216[65:65]), .sbiterra(_216[64:64]), .douta(_216[63:0]) );
    assign _217 = _216[63:0];
    assign _289 = _288[127:64];
    assign data_3 = _289;
    assign address_8 = PHASE_0 ? _199 : _172;
    assign _169 = ~ PHASE_0;
    assign read_enable_6 = _102 & _169;
    assign _166 = ~ _130;
    assign _167 = _129 & _166;
    assign write_enable_8 = _167 & PHASE_0;
    assign _171 = write_enable_8 | read_enable_6;
    assign address_9 = PHASE_0 ? _164 : _137;
    assign _134 = ~ PHASE_0;
    assign read_enable_7 = _102 & _134;
    assign _131 = ~ _130;
    assign _132 = _129 & _131;
    assign write_enable_9 = _132 & PHASE_0;
    assign _136 = write_enable_9 | read_enable_7;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_3
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_136), .regcea(vdd), .wea(write_enable_9), .addra(address_9), .dina(data_7), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_171), .regceb(vdd), .web(write_enable_8), .addrb(address_8), .dinb(data_3), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_202[131:131]), .sbiterrb(_202[130:130]), .doutb(_202[129:66]), .dbiterra(_202[65:65]), .sbiterra(_202[64:64]), .douta(_202[63:0]) );
    assign _203 = _202[63:0];
    assign _290 = ~ PHASE_0;
    assign _5 = _290;
    always @(posedge _84) begin
        if (_82)
            PHASE_0 <= _98;
        else
            if (_99)
                PHASE_0 <= _5;
    end
    assign q0 = PHASE_0 ? _217 : _203;
    always @(posedge _84) begin
        if (_82)
            _96 <= _94;
        else
            _96 <= _92;
    end
    assign _268 = _96 ? _267 : q0;
    dp_6
        dp_6
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_268), .d2(_275), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_288[191:128]), .q2(_288[127:64]), .q1(_288[63:0]) );
    assign _291 = _288[63:0];
    assign data_7 = _291;
    assign address_10 = PHASE_1 ? _164 : _68;
    assign _355 = ~ PHASE_1;
    assign _354 = _70[7:7];
    assign read_enable_8 = _354 & _355;
    always @(posedge _84) begin
        if (_82)
            _325 <= _324;
        else
            _325 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _328 <= _327;
        else
            _328 <= _325;
    end
    always @(posedge _84) begin
        if (_82)
            _331 <= _330;
        else
            _331 <= _328;
    end
    always @(posedge _84) begin
        if (_82)
            _334 <= _333;
        else
            _334 <= _331;
    end
    always @(posedge _84) begin
        if (_82)
            _337 <= _336;
        else
            _337 <= _334;
    end
    always @(posedge _84) begin
        if (_82)
            _340 <= _339;
        else
            _340 <= _337;
    end
    always @(posedge _84) begin
        if (_82)
            _343 <= _342;
        else
            _343 <= _340;
    end
    always @(posedge _84) begin
        if (_82)
            _346 <= _345;
        else
            _346 <= _343;
    end
    always @(posedge _84) begin
        if (_82)
            _349 <= _348;
        else
            _349 <= _346;
    end
    assign _350 = ~ _349;
    always @(posedge _84) begin
        if (_82)
            _298 <= _297;
        else
            _298 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _301 <= _300;
        else
            _301 <= _298;
    end
    always @(posedge _84) begin
        if (_82)
            _304 <= _303;
        else
            _304 <= _301;
    end
    always @(posedge _84) begin
        if (_82)
            _307 <= _306;
        else
            _307 <= _304;
    end
    always @(posedge _84) begin
        if (_82)
            _310 <= _309;
        else
            _310 <= _307;
    end
    always @(posedge _84) begin
        if (_82)
            _313 <= _312;
        else
            _313 <= _310;
    end
    always @(posedge _84) begin
        if (_82)
            _316 <= _315;
        else
            _316 <= _313;
    end
    always @(posedge _84) begin
        if (_82)
            _319 <= _318;
        else
            _319 <= _316;
    end
    always @(posedge _84) begin
        if (_82)
            _322 <= _321;
        else
            _322 <= _319;
    end
    assign _351 = _322 & _350;
    assign _352 = _129 & _351;
    assign write_enable_10 = _352 & PHASE_1;
    assign _357 = write_enable_10 | read_enable_8;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_4
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_357), .regcea(vdd), .wea(write_enable_10), .addra(address_10), .dina(data_7), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_1), .regceb(vdd), .web(write_enable_1), .addrb(address_1), .dinb(data_3), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_364[131:131]), .sbiterrb(_364[130:130]), .doutb(_364[129:66]), .dbiterra(_364[65:65]), .sbiterra(_364[64:64]), .douta(_364[63:0]) );
    assign _365 = _364[63:0];
    assign _295 = ~ PHASE_1;
    assign _7 = _295;
    always @(posedge _84) begin
        if (_82)
            PHASE_1 <= _293;
        else
            if (_72)
                PHASE_1 <= _7;
    end
    assign _377 = PHASE_1 ? _376 : _365;
    assign address_11 = _556 ? _199 : _551;
    assign write_enable_11 = _549 & _556;
    assign address_12 = _556 ? _164 : _68;
    assign _558 = ~ _556;
    assign read_enable_9 = _544 & _558;
    assign _556 = ~ PHASE_4;
    assign write_enable_12 = _542 & _556;
    assign _560 = write_enable_12 | read_enable_9;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_5
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_560), .regcea(vdd), .wea(write_enable_12), .addra(address_12), .dina(data_19), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_11), .regceb(vdd), .web(write_enable_11), .addrb(address_11), .dinb(data_15), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_565[131:131]), .sbiterrb(_565[130:130]), .doutb(_565[129:66]), .dbiterra(_565[65:65]), .sbiterra(_565[64:64]), .douta(_565[63:0]) );
    assign _566 = _565[63:0];
    assign address_13 = PHASE_4 ? _199 : _551;
    assign _549 = _129 & _512;
    assign write_enable_13 = _549 & PHASE_4;
    assign _474 = _466[129:66];
    assign _473 = _453[129:66];
    assign _475 = PHASE_2 ? _474 : _473;
    assign _471 = _417[129:66];
    assign _470 = _403[129:66];
    assign q1_0 = PHASE_3 ? _471 : _470;
    assign _476 = _384 ? _475 : q1_0;
    assign address_14 = _455 ? _449 : _448;
    assign _461 = ~ _455;
    assign read_enable_10 = _102 & _461;
    assign address_15 = _455 ? _60 : _434;
    assign _457 = ~ _455;
    assign read_enable_11 = _102 & _457;
    assign _455 = ~ PHASE_2;
    assign write_enable_15 = _420 & _455;
    assign _459 = write_enable_15 | read_enable_11;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_6
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_459), .regcea(vdd), .wea(write_enable_15), .addra(address_15), .dina(data_13), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_10), .regceb(vdd), .web(write_enable_14), .addrb(address_14), .dinb(data_11), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_466[131:131]), .sbiterrb(_466[130:130]), .doutb(_466[129:66]), .dbiterra(_466[65:65]), .sbiterra(_466[64:64]), .douta(_466[63:0]) );
    assign _467 = _466[63:0];
    assign _447 = _172[8:8];
    assign _446 = _172[7:7];
    assign _445 = _172[6:6];
    assign _444 = _172[5:5];
    assign _443 = _172[4:4];
    assign _442 = _172[3:3];
    assign _441 = _172[2:2];
    assign _440 = _172[1:1];
    assign _439 = _172[0:0];
    assign _448 = { _439, _440, _441, _442, _443, _444, _445, _446, _447 };
    assign address_16 = PHASE_2 ? _449 : _448;
    assign _436 = ~ PHASE_2;
    assign read_enable_12 = _102 & _436;
    assign data_13 = wr_d6;
    assign _433 = _137[8:8];
    assign _432 = _137[7:7];
    assign _431 = _137[6:6];
    assign _430 = _137[5:5];
    assign _429 = _137[4:4];
    assign _428 = _137[3:3];
    assign _427 = _137[2:2];
    assign _426 = _137[1:1];
    assign _425 = _137[0:0];
    assign _434 = { _425, _426, _427, _428, _429, _430, _431, _432, _433 };
    assign address_17 = PHASE_2 ? _60 : _434;
    assign _422 = ~ PHASE_2;
    assign read_enable_13 = _102 & _422;
    assign _420 = _62[6:6];
    assign write_enable_17 = _420 & PHASE_2;
    assign _424 = write_enable_17 | read_enable_13;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_7
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_424), .regcea(vdd), .wea(write_enable_17), .addra(address_17), .dina(data_13), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_12), .regceb(vdd), .web(write_enable_16), .addrb(address_16), .dinb(data_11), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_453[131:131]), .sbiterrb(_453[130:130]), .doutb(_453[129:66]), .dbiterra(_453[65:65]), .sbiterra(_453[64:64]), .douta(_453[63:0]) );
    assign _454 = _453[63:0];
    assign _381 = ~ PHASE_2;
    assign _11 = _381;
    always @(posedge _84) begin
        if (_82)
            PHASE_2 <= _379;
        else
            if (_72)
                PHASE_2 <= _11;
    end
    assign _468 = PHASE_2 ? _467 : _454;
    assign address_18 = _405 ? _199 : _172;
    assign _412 = ~ _405;
    assign read_enable_14 = _102 & _412;
    assign write_enable_18 = _396 & _405;
    assign _414 = write_enable_18 | read_enable_14;
    assign address_19 = _405 ? _164 : _137;
    assign _407 = ~ _405;
    assign read_enable_15 = _102 & _407;
    assign _405 = ~ PHASE_3;
    assign write_enable_19 = _389 & _405;
    assign _409 = write_enable_19 | read_enable_15;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_8
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_409), .regcea(vdd), .wea(write_enable_19), .addra(address_19), .dina(data_19), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_414), .regceb(vdd), .web(write_enable_18), .addrb(address_18), .dinb(data_15), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_417[131:131]), .sbiterrb(_417[130:130]), .doutb(_417[129:66]), .dbiterra(_417[65:65]), .sbiterra(_417[64:64]), .douta(_417[63:0]) );
    assign _418 = _417[63:0];
    assign _479 = _478[127:64];
    assign data_15 = _479;
    assign address_20 = PHASE_3 ? _199 : _172;
    assign _398 = ~ PHASE_3;
    assign read_enable_16 = _102 & _398;
    assign _395 = ~ _130;
    assign _396 = _129 & _395;
    assign write_enable_20 = _396 & PHASE_3;
    assign _400 = write_enable_20 | read_enable_16;
    assign address_21 = PHASE_3 ? _164 : _137;
    assign _391 = ~ PHASE_3;
    assign read_enable_17 = _102 & _391;
    assign _388 = ~ _130;
    assign _389 = _129 & _388;
    assign write_enable_21 = _389 & PHASE_3;
    assign _393 = write_enable_21 | read_enable_17;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_9
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_393), .regcea(vdd), .wea(write_enable_21), .addra(address_21), .dina(data_19), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_400), .regceb(vdd), .web(write_enable_20), .addrb(address_20), .dinb(data_15), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_403[131:131]), .sbiterrb(_403[130:130]), .doutb(_403[129:66]), .dbiterra(_403[65:65]), .sbiterra(_403[64:64]), .douta(_403[63:0]) );
    assign _404 = _403[63:0];
    assign _480 = ~ PHASE_3;
    assign _13 = _480;
    always @(posedge _84) begin
        if (_82)
            PHASE_3 <= _386;
        else
            if (_99)
                PHASE_3 <= _13;
    end
    assign q0_0 = PHASE_3 ? _418 : _404;
    always @(posedge _84) begin
        if (_82)
            _384 <= _383;
        else
            _384 <= _92;
    end
    assign _469 = _384 ? _468 : q0_0;
    dp_5
        dp_5
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_469), .d2(_476), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_478[191:128]), .q2(_478[127:64]), .q1(_478[63:0]) );
    assign _481 = _478[63:0];
    assign data_19 = _481;
    assign address_22 = PHASE_4 ? _164 : _68;
    assign _545 = ~ PHASE_4;
    assign _544 = _70[6:6];
    assign read_enable_18 = _544 & _545;
    always @(posedge _84) begin
        if (_82)
            _515 <= _514;
        else
            _515 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _518 <= _517;
        else
            _518 <= _515;
    end
    always @(posedge _84) begin
        if (_82)
            _521 <= _520;
        else
            _521 <= _518;
    end
    always @(posedge _84) begin
        if (_82)
            _524 <= _523;
        else
            _524 <= _521;
    end
    always @(posedge _84) begin
        if (_82)
            _527 <= _526;
        else
            _527 <= _524;
    end
    always @(posedge _84) begin
        if (_82)
            _530 <= _529;
        else
            _530 <= _527;
    end
    always @(posedge _84) begin
        if (_82)
            _533 <= _532;
        else
            _533 <= _530;
    end
    always @(posedge _84) begin
        if (_82)
            _536 <= _535;
        else
            _536 <= _533;
    end
    always @(posedge _84) begin
        if (_82)
            _539 <= _538;
        else
            _539 <= _536;
    end
    assign _540 = ~ _539;
    always @(posedge _84) begin
        if (_82)
            _488 <= _487;
        else
            _488 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _491 <= _490;
        else
            _491 <= _488;
    end
    always @(posedge _84) begin
        if (_82)
            _494 <= _493;
        else
            _494 <= _491;
    end
    always @(posedge _84) begin
        if (_82)
            _497 <= _496;
        else
            _497 <= _494;
    end
    always @(posedge _84) begin
        if (_82)
            _500 <= _499;
        else
            _500 <= _497;
    end
    always @(posedge _84) begin
        if (_82)
            _503 <= _502;
        else
            _503 <= _500;
    end
    always @(posedge _84) begin
        if (_82)
            _506 <= _505;
        else
            _506 <= _503;
    end
    always @(posedge _84) begin
        if (_82)
            _509 <= _508;
        else
            _509 <= _506;
    end
    always @(posedge _84) begin
        if (_82)
            _512 <= _511;
        else
            _512 <= _509;
    end
    assign _541 = _512 & _540;
    assign _542 = _129 & _541;
    assign write_enable_22 = _542 & PHASE_4;
    assign _547 = write_enable_22 | read_enable_18;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_10
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_547), .regcea(vdd), .wea(write_enable_22), .addra(address_22), .dina(data_19), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_13), .regceb(vdd), .web(write_enable_13), .addrb(address_13), .dinb(data_15), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_554[131:131]), .sbiterrb(_554[130:130]), .doutb(_554[129:66]), .dbiterra(_554[65:65]), .sbiterra(_554[64:64]), .douta(_554[63:0]) );
    assign _555 = _554[63:0];
    assign _485 = ~ PHASE_4;
    assign _15 = _485;
    always @(posedge _84) begin
        if (_82)
            PHASE_4 <= _483;
        else
            if (_72)
                PHASE_4 <= _15;
    end
    assign _567 = PHASE_4 ? _566 : _555;
    assign address_23 = _746 ? _199 : _741;
    assign write_enable_23 = _739 & _746;
    assign address_24 = _746 ? _164 : _68;
    assign _748 = ~ _746;
    assign read_enable_19 = _734 & _748;
    assign _746 = ~ PHASE_7;
    assign write_enable_24 = _732 & _746;
    assign _750 = write_enable_24 | read_enable_19;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_11
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_750), .regcea(vdd), .wea(write_enable_24), .addra(address_24), .dina(data_31), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_23), .regceb(vdd), .web(write_enable_23), .addrb(address_23), .dinb(data_27), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_755[131:131]), .sbiterrb(_755[130:130]), .doutb(_755[129:66]), .dbiterra(_755[65:65]), .sbiterra(_755[64:64]), .douta(_755[63:0]) );
    assign _756 = _755[63:0];
    assign address_25 = PHASE_7 ? _199 : _741;
    assign _739 = _129 & _702;
    assign write_enable_25 = _739 & PHASE_7;
    assign _664 = _656[129:66];
    assign _663 = _643[129:66];
    assign _665 = PHASE_5 ? _664 : _663;
    assign _661 = _607[129:66];
    assign _660 = _593[129:66];
    assign q1_1 = PHASE_6 ? _661 : _660;
    assign _666 = _574 ? _665 : q1_1;
    assign address_26 = _645 ? _639 : _638;
    assign _651 = ~ _645;
    assign read_enable_20 = _102 & _651;
    assign address_27 = _645 ? _60 : _624;
    assign _647 = ~ _645;
    assign read_enable_21 = _102 & _647;
    assign _645 = ~ PHASE_5;
    assign write_enable_27 = _610 & _645;
    assign _649 = write_enable_27 | read_enable_21;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_12
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_649), .regcea(vdd), .wea(write_enable_27), .addra(address_27), .dina(data_25), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_20), .regceb(vdd), .web(write_enable_26), .addrb(address_26), .dinb(data_23), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_656[131:131]), .sbiterrb(_656[130:130]), .doutb(_656[129:66]), .dbiterra(_656[65:65]), .sbiterra(_656[64:64]), .douta(_656[63:0]) );
    assign _657 = _656[63:0];
    assign _637 = _172[8:8];
    assign _636 = _172[7:7];
    assign _635 = _172[6:6];
    assign _634 = _172[5:5];
    assign _633 = _172[4:4];
    assign _632 = _172[3:3];
    assign _631 = _172[2:2];
    assign _630 = _172[1:1];
    assign _629 = _172[0:0];
    assign _638 = { _629, _630, _631, _632, _633, _634, _635, _636, _637 };
    assign address_28 = PHASE_5 ? _639 : _638;
    assign _626 = ~ PHASE_5;
    assign read_enable_22 = _102 & _626;
    assign data_25 = wr_d5;
    assign _623 = _137[8:8];
    assign _622 = _137[7:7];
    assign _621 = _137[6:6];
    assign _620 = _137[5:5];
    assign _619 = _137[4:4];
    assign _618 = _137[3:3];
    assign _617 = _137[2:2];
    assign _616 = _137[1:1];
    assign _615 = _137[0:0];
    assign _624 = { _615, _616, _617, _618, _619, _620, _621, _622, _623 };
    assign address_29 = PHASE_5 ? _60 : _624;
    assign _612 = ~ PHASE_5;
    assign read_enable_23 = _102 & _612;
    assign _610 = _62[5:5];
    assign write_enable_29 = _610 & PHASE_5;
    assign _614 = write_enable_29 | read_enable_23;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_13
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_614), .regcea(vdd), .wea(write_enable_29), .addra(address_29), .dina(data_25), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_22), .regceb(vdd), .web(write_enable_28), .addrb(address_28), .dinb(data_23), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_643[131:131]), .sbiterrb(_643[130:130]), .doutb(_643[129:66]), .dbiterra(_643[65:65]), .sbiterra(_643[64:64]), .douta(_643[63:0]) );
    assign _644 = _643[63:0];
    assign _571 = ~ PHASE_5;
    assign _19 = _571;
    always @(posedge _84) begin
        if (_82)
            PHASE_5 <= _569;
        else
            if (_72)
                PHASE_5 <= _19;
    end
    assign _658 = PHASE_5 ? _657 : _644;
    assign address_30 = _595 ? _199 : _172;
    assign _602 = ~ _595;
    assign read_enable_24 = _102 & _602;
    assign write_enable_30 = _586 & _595;
    assign _604 = write_enable_30 | read_enable_24;
    assign address_31 = _595 ? _164 : _137;
    assign _597 = ~ _595;
    assign read_enable_25 = _102 & _597;
    assign _595 = ~ PHASE_6;
    assign write_enable_31 = _579 & _595;
    assign _599 = write_enable_31 | read_enable_25;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_14
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_599), .regcea(vdd), .wea(write_enable_31), .addra(address_31), .dina(data_31), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_604), .regceb(vdd), .web(write_enable_30), .addrb(address_30), .dinb(data_27), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_607[131:131]), .sbiterrb(_607[130:130]), .doutb(_607[129:66]), .dbiterra(_607[65:65]), .sbiterra(_607[64:64]), .douta(_607[63:0]) );
    assign _608 = _607[63:0];
    assign _669 = _668[127:64];
    assign data_27 = _669;
    assign address_32 = PHASE_6 ? _199 : _172;
    assign _588 = ~ PHASE_6;
    assign read_enable_26 = _102 & _588;
    assign _585 = ~ _130;
    assign _586 = _129 & _585;
    assign write_enable_32 = _586 & PHASE_6;
    assign _590 = write_enable_32 | read_enable_26;
    assign address_33 = PHASE_6 ? _164 : _137;
    assign _581 = ~ PHASE_6;
    assign read_enable_27 = _102 & _581;
    assign _578 = ~ _130;
    assign _579 = _129 & _578;
    assign write_enable_33 = _579 & PHASE_6;
    assign _583 = write_enable_33 | read_enable_27;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_15
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_583), .regcea(vdd), .wea(write_enable_33), .addra(address_33), .dina(data_31), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_590), .regceb(vdd), .web(write_enable_32), .addrb(address_32), .dinb(data_27), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_593[131:131]), .sbiterrb(_593[130:130]), .doutb(_593[129:66]), .dbiterra(_593[65:65]), .sbiterra(_593[64:64]), .douta(_593[63:0]) );
    assign _594 = _593[63:0];
    assign _670 = ~ PHASE_6;
    assign _21 = _670;
    always @(posedge _84) begin
        if (_82)
            PHASE_6 <= _576;
        else
            if (_99)
                PHASE_6 <= _21;
    end
    assign q0_1 = PHASE_6 ? _608 : _594;
    always @(posedge _84) begin
        if (_82)
            _574 <= _573;
        else
            _574 <= _92;
    end
    assign _659 = _574 ? _658 : q0_1;
    dp_4
        dp_4
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_659), .d2(_666), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_668[191:128]), .q2(_668[127:64]), .q1(_668[63:0]) );
    assign _671 = _668[63:0];
    assign data_31 = _671;
    assign address_34 = PHASE_7 ? _164 : _68;
    assign _735 = ~ PHASE_7;
    assign _734 = _70[5:5];
    assign read_enable_28 = _734 & _735;
    always @(posedge _84) begin
        if (_82)
            _705 <= _704;
        else
            _705 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _708 <= _707;
        else
            _708 <= _705;
    end
    always @(posedge _84) begin
        if (_82)
            _711 <= _710;
        else
            _711 <= _708;
    end
    always @(posedge _84) begin
        if (_82)
            _714 <= _713;
        else
            _714 <= _711;
    end
    always @(posedge _84) begin
        if (_82)
            _717 <= _716;
        else
            _717 <= _714;
    end
    always @(posedge _84) begin
        if (_82)
            _720 <= _719;
        else
            _720 <= _717;
    end
    always @(posedge _84) begin
        if (_82)
            _723 <= _722;
        else
            _723 <= _720;
    end
    always @(posedge _84) begin
        if (_82)
            _726 <= _725;
        else
            _726 <= _723;
    end
    always @(posedge _84) begin
        if (_82)
            _729 <= _728;
        else
            _729 <= _726;
    end
    assign _730 = ~ _729;
    always @(posedge _84) begin
        if (_82)
            _678 <= _677;
        else
            _678 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _681 <= _680;
        else
            _681 <= _678;
    end
    always @(posedge _84) begin
        if (_82)
            _684 <= _683;
        else
            _684 <= _681;
    end
    always @(posedge _84) begin
        if (_82)
            _687 <= _686;
        else
            _687 <= _684;
    end
    always @(posedge _84) begin
        if (_82)
            _690 <= _689;
        else
            _690 <= _687;
    end
    always @(posedge _84) begin
        if (_82)
            _693 <= _692;
        else
            _693 <= _690;
    end
    always @(posedge _84) begin
        if (_82)
            _696 <= _695;
        else
            _696 <= _693;
    end
    always @(posedge _84) begin
        if (_82)
            _699 <= _698;
        else
            _699 <= _696;
    end
    always @(posedge _84) begin
        if (_82)
            _702 <= _701;
        else
            _702 <= _699;
    end
    assign _731 = _702 & _730;
    assign _732 = _129 & _731;
    assign write_enable_34 = _732 & PHASE_7;
    assign _737 = write_enable_34 | read_enable_28;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_16
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_737), .regcea(vdd), .wea(write_enable_34), .addra(address_34), .dina(data_31), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_25), .regceb(vdd), .web(write_enable_25), .addrb(address_25), .dinb(data_27), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_744[131:131]), .sbiterrb(_744[130:130]), .doutb(_744[129:66]), .dbiterra(_744[65:65]), .sbiterra(_744[64:64]), .douta(_744[63:0]) );
    assign _745 = _744[63:0];
    assign _675 = ~ PHASE_7;
    assign _23 = _675;
    always @(posedge _84) begin
        if (_82)
            PHASE_7 <= _673;
        else
            if (_72)
                PHASE_7 <= _23;
    end
    assign _757 = PHASE_7 ? _756 : _745;
    assign address_35 = _936 ? _199 : _931;
    assign write_enable_35 = _929 & _936;
    assign address_36 = _936 ? _164 : _68;
    assign _938 = ~ _936;
    assign read_enable_29 = _924 & _938;
    assign _936 = ~ PHASE_10;
    assign write_enable_36 = _922 & _936;
    assign _940 = write_enable_36 | read_enable_29;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_17
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_940), .regcea(vdd), .wea(write_enable_36), .addra(address_36), .dina(data_43), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_35), .regceb(vdd), .web(write_enable_35), .addrb(address_35), .dinb(data_39), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_945[131:131]), .sbiterrb(_945[130:130]), .doutb(_945[129:66]), .dbiterra(_945[65:65]), .sbiterra(_945[64:64]), .douta(_945[63:0]) );
    assign _946 = _945[63:0];
    assign address_37 = PHASE_10 ? _199 : _931;
    assign _929 = _129 & _892;
    assign write_enable_37 = _929 & PHASE_10;
    assign _854 = _846[129:66];
    assign _853 = _833[129:66];
    assign _855 = PHASE_8 ? _854 : _853;
    assign _851 = _797[129:66];
    assign _850 = _783[129:66];
    assign q1_2 = PHASE_9 ? _851 : _850;
    assign _856 = _764 ? _855 : q1_2;
    assign address_38 = _835 ? _829 : _828;
    assign _841 = ~ _835;
    assign read_enable_30 = _102 & _841;
    assign address_39 = _835 ? _60 : _814;
    assign _837 = ~ _835;
    assign read_enable_31 = _102 & _837;
    assign _835 = ~ PHASE_8;
    assign write_enable_39 = _800 & _835;
    assign _839 = write_enable_39 | read_enable_31;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_18
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_839), .regcea(vdd), .wea(write_enable_39), .addra(address_39), .dina(data_37), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_30), .regceb(vdd), .web(write_enable_38), .addrb(address_38), .dinb(data_35), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_846[131:131]), .sbiterrb(_846[130:130]), .doutb(_846[129:66]), .dbiterra(_846[65:65]), .sbiterra(_846[64:64]), .douta(_846[63:0]) );
    assign _847 = _846[63:0];
    assign _827 = _172[8:8];
    assign _826 = _172[7:7];
    assign _825 = _172[6:6];
    assign _824 = _172[5:5];
    assign _823 = _172[4:4];
    assign _822 = _172[3:3];
    assign _821 = _172[2:2];
    assign _820 = _172[1:1];
    assign _819 = _172[0:0];
    assign _828 = { _819, _820, _821, _822, _823, _824, _825, _826, _827 };
    assign address_40 = PHASE_8 ? _829 : _828;
    assign _816 = ~ PHASE_8;
    assign read_enable_32 = _102 & _816;
    assign data_37 = wr_d4;
    assign _813 = _137[8:8];
    assign _812 = _137[7:7];
    assign _811 = _137[6:6];
    assign _810 = _137[5:5];
    assign _809 = _137[4:4];
    assign _808 = _137[3:3];
    assign _807 = _137[2:2];
    assign _806 = _137[1:1];
    assign _805 = _137[0:0];
    assign _814 = { _805, _806, _807, _808, _809, _810, _811, _812, _813 };
    assign address_41 = PHASE_8 ? _60 : _814;
    assign _802 = ~ PHASE_8;
    assign read_enable_33 = _102 & _802;
    assign _800 = _62[4:4];
    assign write_enable_41 = _800 & PHASE_8;
    assign _804 = write_enable_41 | read_enable_33;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_19
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_804), .regcea(vdd), .wea(write_enable_41), .addra(address_41), .dina(data_37), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_32), .regceb(vdd), .web(write_enable_40), .addrb(address_40), .dinb(data_35), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_833[131:131]), .sbiterrb(_833[130:130]), .doutb(_833[129:66]), .dbiterra(_833[65:65]), .sbiterra(_833[64:64]), .douta(_833[63:0]) );
    assign _834 = _833[63:0];
    assign _761 = ~ PHASE_8;
    assign _27 = _761;
    always @(posedge _84) begin
        if (_82)
            PHASE_8 <= _759;
        else
            if (_72)
                PHASE_8 <= _27;
    end
    assign _848 = PHASE_8 ? _847 : _834;
    assign address_42 = _785 ? _199 : _172;
    assign _792 = ~ _785;
    assign read_enable_34 = _102 & _792;
    assign write_enable_42 = _776 & _785;
    assign _794 = write_enable_42 | read_enable_34;
    assign address_43 = _785 ? _164 : _137;
    assign _787 = ~ _785;
    assign read_enable_35 = _102 & _787;
    assign _785 = ~ PHASE_9;
    assign write_enable_43 = _769 & _785;
    assign _789 = write_enable_43 | read_enable_35;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_20
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_789), .regcea(vdd), .wea(write_enable_43), .addra(address_43), .dina(data_43), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_794), .regceb(vdd), .web(write_enable_42), .addrb(address_42), .dinb(data_39), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_797[131:131]), .sbiterrb(_797[130:130]), .doutb(_797[129:66]), .dbiterra(_797[65:65]), .sbiterra(_797[64:64]), .douta(_797[63:0]) );
    assign _798 = _797[63:0];
    assign _859 = _858[127:64];
    assign data_39 = _859;
    assign address_44 = PHASE_9 ? _199 : _172;
    assign _778 = ~ PHASE_9;
    assign read_enable_36 = _102 & _778;
    assign _775 = ~ _130;
    assign _776 = _129 & _775;
    assign write_enable_44 = _776 & PHASE_9;
    assign _780 = write_enable_44 | read_enable_36;
    assign address_45 = PHASE_9 ? _164 : _137;
    assign _771 = ~ PHASE_9;
    assign read_enable_37 = _102 & _771;
    assign _768 = ~ _130;
    assign _769 = _129 & _768;
    assign write_enable_45 = _769 & PHASE_9;
    assign _773 = write_enable_45 | read_enable_37;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_21
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_773), .regcea(vdd), .wea(write_enable_45), .addra(address_45), .dina(data_43), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_780), .regceb(vdd), .web(write_enable_44), .addrb(address_44), .dinb(data_39), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_783[131:131]), .sbiterrb(_783[130:130]), .doutb(_783[129:66]), .dbiterra(_783[65:65]), .sbiterra(_783[64:64]), .douta(_783[63:0]) );
    assign _784 = _783[63:0];
    assign _860 = ~ PHASE_9;
    assign _29 = _860;
    always @(posedge _84) begin
        if (_82)
            PHASE_9 <= _766;
        else
            if (_99)
                PHASE_9 <= _29;
    end
    assign q0_2 = PHASE_9 ? _798 : _784;
    always @(posedge _84) begin
        if (_82)
            _764 <= _763;
        else
            _764 <= _92;
    end
    assign _849 = _764 ? _848 : q0_2;
    dp_3
        dp_3
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_849), .d2(_856), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_858[191:128]), .q2(_858[127:64]), .q1(_858[63:0]) );
    assign _861 = _858[63:0];
    assign data_43 = _861;
    assign address_46 = PHASE_10 ? _164 : _68;
    assign _925 = ~ PHASE_10;
    assign _924 = _70[4:4];
    assign read_enable_38 = _924 & _925;
    always @(posedge _84) begin
        if (_82)
            _895 <= _894;
        else
            _895 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _898 <= _897;
        else
            _898 <= _895;
    end
    always @(posedge _84) begin
        if (_82)
            _901 <= _900;
        else
            _901 <= _898;
    end
    always @(posedge _84) begin
        if (_82)
            _904 <= _903;
        else
            _904 <= _901;
    end
    always @(posedge _84) begin
        if (_82)
            _907 <= _906;
        else
            _907 <= _904;
    end
    always @(posedge _84) begin
        if (_82)
            _910 <= _909;
        else
            _910 <= _907;
    end
    always @(posedge _84) begin
        if (_82)
            _913 <= _912;
        else
            _913 <= _910;
    end
    always @(posedge _84) begin
        if (_82)
            _916 <= _915;
        else
            _916 <= _913;
    end
    always @(posedge _84) begin
        if (_82)
            _919 <= _918;
        else
            _919 <= _916;
    end
    assign _920 = ~ _919;
    always @(posedge _84) begin
        if (_82)
            _868 <= _867;
        else
            _868 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _871 <= _870;
        else
            _871 <= _868;
    end
    always @(posedge _84) begin
        if (_82)
            _874 <= _873;
        else
            _874 <= _871;
    end
    always @(posedge _84) begin
        if (_82)
            _877 <= _876;
        else
            _877 <= _874;
    end
    always @(posedge _84) begin
        if (_82)
            _880 <= _879;
        else
            _880 <= _877;
    end
    always @(posedge _84) begin
        if (_82)
            _883 <= _882;
        else
            _883 <= _880;
    end
    always @(posedge _84) begin
        if (_82)
            _886 <= _885;
        else
            _886 <= _883;
    end
    always @(posedge _84) begin
        if (_82)
            _889 <= _888;
        else
            _889 <= _886;
    end
    always @(posedge _84) begin
        if (_82)
            _892 <= _891;
        else
            _892 <= _889;
    end
    assign _921 = _892 & _920;
    assign _922 = _129 & _921;
    assign write_enable_46 = _922 & PHASE_10;
    assign _927 = write_enable_46 | read_enable_38;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_22
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_927), .regcea(vdd), .wea(write_enable_46), .addra(address_46), .dina(data_43), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_37), .regceb(vdd), .web(write_enable_37), .addrb(address_37), .dinb(data_39), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_934[131:131]), .sbiterrb(_934[130:130]), .doutb(_934[129:66]), .dbiterra(_934[65:65]), .sbiterra(_934[64:64]), .douta(_934[63:0]) );
    assign _935 = _934[63:0];
    assign _865 = ~ PHASE_10;
    assign _31 = _865;
    always @(posedge _84) begin
        if (_82)
            PHASE_10 <= _863;
        else
            if (_72)
                PHASE_10 <= _31;
    end
    assign _947 = PHASE_10 ? _946 : _935;
    assign address_47 = _1126 ? _199 : _1121;
    assign write_enable_47 = _1119 & _1126;
    assign address_48 = _1126 ? _164 : _68;
    assign _1128 = ~ _1126;
    assign read_enable_39 = _1114 & _1128;
    assign _1126 = ~ PHASE_13;
    assign write_enable_48 = _1112 & _1126;
    assign _1130 = write_enable_48 | read_enable_39;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_23
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1130), .regcea(vdd), .wea(write_enable_48), .addra(address_48), .dina(data_55), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_47), .regceb(vdd), .web(write_enable_47), .addrb(address_47), .dinb(data_51), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1135[131:131]), .sbiterrb(_1135[130:130]), .doutb(_1135[129:66]), .dbiterra(_1135[65:65]), .sbiterra(_1135[64:64]), .douta(_1135[63:0]) );
    assign _1136 = _1135[63:0];
    assign address_49 = PHASE_13 ? _199 : _1121;
    assign _1119 = _129 & _1082;
    assign write_enable_49 = _1119 & PHASE_13;
    assign _1044 = _1036[129:66];
    assign _1043 = _1023[129:66];
    assign _1045 = PHASE_11 ? _1044 : _1043;
    assign _1041 = _987[129:66];
    assign _1040 = _973[129:66];
    assign q1_3 = PHASE_12 ? _1041 : _1040;
    assign _1046 = _954 ? _1045 : q1_3;
    assign address_50 = _1025 ? _1019 : _1018;
    assign _1031 = ~ _1025;
    assign read_enable_40 = _102 & _1031;
    assign address_51 = _1025 ? _60 : _1004;
    assign _1027 = ~ _1025;
    assign read_enable_41 = _102 & _1027;
    assign _1025 = ~ PHASE_11;
    assign write_enable_51 = _990 & _1025;
    assign _1029 = write_enable_51 | read_enable_41;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_24
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1029), .regcea(vdd), .wea(write_enable_51), .addra(address_51), .dina(data_49), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_40), .regceb(vdd), .web(write_enable_50), .addrb(address_50), .dinb(data_47), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1036[131:131]), .sbiterrb(_1036[130:130]), .doutb(_1036[129:66]), .dbiterra(_1036[65:65]), .sbiterra(_1036[64:64]), .douta(_1036[63:0]) );
    assign _1037 = _1036[63:0];
    assign _1017 = _172[8:8];
    assign _1016 = _172[7:7];
    assign _1015 = _172[6:6];
    assign _1014 = _172[5:5];
    assign _1013 = _172[4:4];
    assign _1012 = _172[3:3];
    assign _1011 = _172[2:2];
    assign _1010 = _172[1:1];
    assign _1009 = _172[0:0];
    assign _1018 = { _1009, _1010, _1011, _1012, _1013, _1014, _1015, _1016, _1017 };
    assign address_52 = PHASE_11 ? _1019 : _1018;
    assign _1006 = ~ PHASE_11;
    assign read_enable_42 = _102 & _1006;
    assign data_49 = wr_d3;
    assign _1003 = _137[8:8];
    assign _1002 = _137[7:7];
    assign _1001 = _137[6:6];
    assign _1000 = _137[5:5];
    assign _999 = _137[4:4];
    assign _998 = _137[3:3];
    assign _997 = _137[2:2];
    assign _996 = _137[1:1];
    assign _995 = _137[0:0];
    assign _1004 = { _995, _996, _997, _998, _999, _1000, _1001, _1002, _1003 };
    assign address_53 = PHASE_11 ? _60 : _1004;
    assign _992 = ~ PHASE_11;
    assign read_enable_43 = _102 & _992;
    assign _990 = _62[3:3];
    assign write_enable_53 = _990 & PHASE_11;
    assign _994 = write_enable_53 | read_enable_43;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_25
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_994), .regcea(vdd), .wea(write_enable_53), .addra(address_53), .dina(data_49), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_42), .regceb(vdd), .web(write_enable_52), .addrb(address_52), .dinb(data_47), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1023[131:131]), .sbiterrb(_1023[130:130]), .doutb(_1023[129:66]), .dbiterra(_1023[65:65]), .sbiterra(_1023[64:64]), .douta(_1023[63:0]) );
    assign _1024 = _1023[63:0];
    assign _951 = ~ PHASE_11;
    assign _35 = _951;
    always @(posedge _84) begin
        if (_82)
            PHASE_11 <= _949;
        else
            if (_72)
                PHASE_11 <= _35;
    end
    assign _1038 = PHASE_11 ? _1037 : _1024;
    assign address_54 = _975 ? _199 : _172;
    assign _982 = ~ _975;
    assign read_enable_44 = _102 & _982;
    assign write_enable_54 = _966 & _975;
    assign _984 = write_enable_54 | read_enable_44;
    assign address_55 = _975 ? _164 : _137;
    assign _977 = ~ _975;
    assign read_enable_45 = _102 & _977;
    assign _975 = ~ PHASE_12;
    assign write_enable_55 = _959 & _975;
    assign _979 = write_enable_55 | read_enable_45;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_26
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_979), .regcea(vdd), .wea(write_enable_55), .addra(address_55), .dina(data_55), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_984), .regceb(vdd), .web(write_enable_54), .addrb(address_54), .dinb(data_51), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_987[131:131]), .sbiterrb(_987[130:130]), .doutb(_987[129:66]), .dbiterra(_987[65:65]), .sbiterra(_987[64:64]), .douta(_987[63:0]) );
    assign _988 = _987[63:0];
    assign _1049 = _1048[127:64];
    assign data_51 = _1049;
    assign address_56 = PHASE_12 ? _199 : _172;
    assign _968 = ~ PHASE_12;
    assign read_enable_46 = _102 & _968;
    assign _965 = ~ _130;
    assign _966 = _129 & _965;
    assign write_enable_56 = _966 & PHASE_12;
    assign _970 = write_enable_56 | read_enable_46;
    assign address_57 = PHASE_12 ? _164 : _137;
    assign _961 = ~ PHASE_12;
    assign read_enable_47 = _102 & _961;
    assign _958 = ~ _130;
    assign _959 = _129 & _958;
    assign write_enable_57 = _959 & PHASE_12;
    assign _963 = write_enable_57 | read_enable_47;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_27
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_963), .regcea(vdd), .wea(write_enable_57), .addra(address_57), .dina(data_55), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_970), .regceb(vdd), .web(write_enable_56), .addrb(address_56), .dinb(data_51), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_973[131:131]), .sbiterrb(_973[130:130]), .doutb(_973[129:66]), .dbiterra(_973[65:65]), .sbiterra(_973[64:64]), .douta(_973[63:0]) );
    assign _974 = _973[63:0];
    assign _1050 = ~ PHASE_12;
    assign _37 = _1050;
    always @(posedge _84) begin
        if (_82)
            PHASE_12 <= _956;
        else
            if (_99)
                PHASE_12 <= _37;
    end
    assign q0_3 = PHASE_12 ? _988 : _974;
    always @(posedge _84) begin
        if (_82)
            _954 <= _953;
        else
            _954 <= _92;
    end
    assign _1039 = _954 ? _1038 : q0_3;
    dp_2
        dp_2
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_1039), .d2(_1046), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_1048[191:128]), .q2(_1048[127:64]), .q1(_1048[63:0]) );
    assign _1051 = _1048[63:0];
    assign data_55 = _1051;
    assign address_58 = PHASE_13 ? _164 : _68;
    assign _1115 = ~ PHASE_13;
    assign _1114 = _70[3:3];
    assign read_enable_48 = _1114 & _1115;
    always @(posedge _84) begin
        if (_82)
            _1085 <= _1084;
        else
            _1085 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _1088 <= _1087;
        else
            _1088 <= _1085;
    end
    always @(posedge _84) begin
        if (_82)
            _1091 <= _1090;
        else
            _1091 <= _1088;
    end
    always @(posedge _84) begin
        if (_82)
            _1094 <= _1093;
        else
            _1094 <= _1091;
    end
    always @(posedge _84) begin
        if (_82)
            _1097 <= _1096;
        else
            _1097 <= _1094;
    end
    always @(posedge _84) begin
        if (_82)
            _1100 <= _1099;
        else
            _1100 <= _1097;
    end
    always @(posedge _84) begin
        if (_82)
            _1103 <= _1102;
        else
            _1103 <= _1100;
    end
    always @(posedge _84) begin
        if (_82)
            _1106 <= _1105;
        else
            _1106 <= _1103;
    end
    always @(posedge _84) begin
        if (_82)
            _1109 <= _1108;
        else
            _1109 <= _1106;
    end
    assign _1110 = ~ _1109;
    always @(posedge _84) begin
        if (_82)
            _1058 <= _1057;
        else
            _1058 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _1061 <= _1060;
        else
            _1061 <= _1058;
    end
    always @(posedge _84) begin
        if (_82)
            _1064 <= _1063;
        else
            _1064 <= _1061;
    end
    always @(posedge _84) begin
        if (_82)
            _1067 <= _1066;
        else
            _1067 <= _1064;
    end
    always @(posedge _84) begin
        if (_82)
            _1070 <= _1069;
        else
            _1070 <= _1067;
    end
    always @(posedge _84) begin
        if (_82)
            _1073 <= _1072;
        else
            _1073 <= _1070;
    end
    always @(posedge _84) begin
        if (_82)
            _1076 <= _1075;
        else
            _1076 <= _1073;
    end
    always @(posedge _84) begin
        if (_82)
            _1079 <= _1078;
        else
            _1079 <= _1076;
    end
    always @(posedge _84) begin
        if (_82)
            _1082 <= _1081;
        else
            _1082 <= _1079;
    end
    assign _1111 = _1082 & _1110;
    assign _1112 = _129 & _1111;
    assign write_enable_58 = _1112 & PHASE_13;
    assign _1117 = write_enable_58 | read_enable_48;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_28
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1117), .regcea(vdd), .wea(write_enable_58), .addra(address_58), .dina(data_55), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_49), .regceb(vdd), .web(write_enable_49), .addrb(address_49), .dinb(data_51), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1124[131:131]), .sbiterrb(_1124[130:130]), .doutb(_1124[129:66]), .dbiterra(_1124[65:65]), .sbiterra(_1124[64:64]), .douta(_1124[63:0]) );
    assign _1125 = _1124[63:0];
    assign _1055 = ~ PHASE_13;
    assign _39 = _1055;
    always @(posedge _84) begin
        if (_82)
            PHASE_13 <= _1053;
        else
            if (_72)
                PHASE_13 <= _39;
    end
    assign _1137 = PHASE_13 ? _1136 : _1125;
    assign address_59 = _1316 ? _199 : _1311;
    assign write_enable_59 = _1309 & _1316;
    assign address_60 = _1316 ? _164 : _68;
    assign _1318 = ~ _1316;
    assign read_enable_49 = _1304 & _1318;
    assign _1316 = ~ PHASE_16;
    assign write_enable_60 = _1302 & _1316;
    assign _1320 = write_enable_60 | read_enable_49;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_29
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1320), .regcea(vdd), .wea(write_enable_60), .addra(address_60), .dina(data_67), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_59), .regceb(vdd), .web(write_enable_59), .addrb(address_59), .dinb(data_63), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1325[131:131]), .sbiterrb(_1325[130:130]), .doutb(_1325[129:66]), .dbiterra(_1325[65:65]), .sbiterra(_1325[64:64]), .douta(_1325[63:0]) );
    assign _1326 = _1325[63:0];
    assign address_61 = PHASE_16 ? _199 : _1311;
    assign _1309 = _129 & _1272;
    assign write_enable_61 = _1309 & PHASE_16;
    assign _1234 = _1226[129:66];
    assign _1233 = _1213[129:66];
    assign _1235 = PHASE_14 ? _1234 : _1233;
    assign _1231 = _1177[129:66];
    assign _1230 = _1163[129:66];
    assign q1_4 = PHASE_15 ? _1231 : _1230;
    assign _1236 = _1144 ? _1235 : q1_4;
    assign address_62 = _1215 ? _1209 : _1208;
    assign _1221 = ~ _1215;
    assign read_enable_50 = _102 & _1221;
    assign address_63 = _1215 ? _60 : _1194;
    assign _1217 = ~ _1215;
    assign read_enable_51 = _102 & _1217;
    assign _1215 = ~ PHASE_14;
    assign write_enable_63 = _1180 & _1215;
    assign _1219 = write_enable_63 | read_enable_51;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_30
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1219), .regcea(vdd), .wea(write_enable_63), .addra(address_63), .dina(data_61), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_50), .regceb(vdd), .web(write_enable_62), .addrb(address_62), .dinb(data_59), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1226[131:131]), .sbiterrb(_1226[130:130]), .doutb(_1226[129:66]), .dbiterra(_1226[65:65]), .sbiterra(_1226[64:64]), .douta(_1226[63:0]) );
    assign _1227 = _1226[63:0];
    assign _1207 = _172[8:8];
    assign _1206 = _172[7:7];
    assign _1205 = _172[6:6];
    assign _1204 = _172[5:5];
    assign _1203 = _172[4:4];
    assign _1202 = _172[3:3];
    assign _1201 = _172[2:2];
    assign _1200 = _172[1:1];
    assign _1199 = _172[0:0];
    assign _1208 = { _1199, _1200, _1201, _1202, _1203, _1204, _1205, _1206, _1207 };
    assign address_64 = PHASE_14 ? _1209 : _1208;
    assign _1196 = ~ PHASE_14;
    assign read_enable_52 = _102 & _1196;
    assign data_61 = wr_d2;
    assign _1193 = _137[8:8];
    assign _1192 = _137[7:7];
    assign _1191 = _137[6:6];
    assign _1190 = _137[5:5];
    assign _1189 = _137[4:4];
    assign _1188 = _137[3:3];
    assign _1187 = _137[2:2];
    assign _1186 = _137[1:1];
    assign _1185 = _137[0:0];
    assign _1194 = { _1185, _1186, _1187, _1188, _1189, _1190, _1191, _1192, _1193 };
    assign address_65 = PHASE_14 ? _60 : _1194;
    assign _1182 = ~ PHASE_14;
    assign read_enable_53 = _102 & _1182;
    assign _1180 = _62[2:2];
    assign write_enable_65 = _1180 & PHASE_14;
    assign _1184 = write_enable_65 | read_enable_53;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_31
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1184), .regcea(vdd), .wea(write_enable_65), .addra(address_65), .dina(data_61), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_52), .regceb(vdd), .web(write_enable_64), .addrb(address_64), .dinb(data_59), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1213[131:131]), .sbiterrb(_1213[130:130]), .doutb(_1213[129:66]), .dbiterra(_1213[65:65]), .sbiterra(_1213[64:64]), .douta(_1213[63:0]) );
    assign _1214 = _1213[63:0];
    assign _1141 = ~ PHASE_14;
    assign _43 = _1141;
    always @(posedge _84) begin
        if (_82)
            PHASE_14 <= _1139;
        else
            if (_72)
                PHASE_14 <= _43;
    end
    assign _1228 = PHASE_14 ? _1227 : _1214;
    assign address_66 = _1165 ? _199 : _172;
    assign _1172 = ~ _1165;
    assign read_enable_54 = _102 & _1172;
    assign write_enable_66 = _1156 & _1165;
    assign _1174 = write_enable_66 | read_enable_54;
    assign address_67 = _1165 ? _164 : _137;
    assign _1167 = ~ _1165;
    assign read_enable_55 = _102 & _1167;
    assign _1165 = ~ PHASE_15;
    assign write_enable_67 = _1149 & _1165;
    assign _1169 = write_enable_67 | read_enable_55;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_32
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1169), .regcea(vdd), .wea(write_enable_67), .addra(address_67), .dina(data_67), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_1174), .regceb(vdd), .web(write_enable_66), .addrb(address_66), .dinb(data_63), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1177[131:131]), .sbiterrb(_1177[130:130]), .doutb(_1177[129:66]), .dbiterra(_1177[65:65]), .sbiterra(_1177[64:64]), .douta(_1177[63:0]) );
    assign _1178 = _1177[63:0];
    assign _1239 = _1238[127:64];
    assign data_63 = _1239;
    assign address_68 = PHASE_15 ? _199 : _172;
    assign _1158 = ~ PHASE_15;
    assign read_enable_56 = _102 & _1158;
    assign _1155 = ~ _130;
    assign _1156 = _129 & _1155;
    assign write_enable_68 = _1156 & PHASE_15;
    assign _1160 = write_enable_68 | read_enable_56;
    assign address_69 = PHASE_15 ? _164 : _137;
    assign _1151 = ~ PHASE_15;
    assign read_enable_57 = _102 & _1151;
    assign _1148 = ~ _130;
    assign _1149 = _129 & _1148;
    assign write_enable_69 = _1149 & PHASE_15;
    assign _1153 = write_enable_69 | read_enable_57;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_33
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1153), .regcea(vdd), .wea(write_enable_69), .addra(address_69), .dina(data_67), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_1160), .regceb(vdd), .web(write_enable_68), .addrb(address_68), .dinb(data_63), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1163[131:131]), .sbiterrb(_1163[130:130]), .doutb(_1163[129:66]), .dbiterra(_1163[65:65]), .sbiterra(_1163[64:64]), .douta(_1163[63:0]) );
    assign _1164 = _1163[63:0];
    assign _1240 = ~ PHASE_15;
    assign _45 = _1240;
    always @(posedge _84) begin
        if (_82)
            PHASE_15 <= _1146;
        else
            if (_99)
                PHASE_15 <= _45;
    end
    assign q0_4 = PHASE_15 ? _1178 : _1164;
    always @(posedge _84) begin
        if (_82)
            _1144 <= _1143;
        else
            _1144 <= _92;
    end
    assign _1229 = _1144 ? _1228 : q0_4;
    dp_1
        dp_1
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_1229), .d2(_1236), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_1238[191:128]), .q2(_1238[127:64]), .q1(_1238[63:0]) );
    assign _1241 = _1238[63:0];
    assign data_67 = _1241;
    assign address_70 = PHASE_16 ? _164 : _68;
    assign _1305 = ~ PHASE_16;
    assign _1304 = _70[2:2];
    assign read_enable_58 = _1304 & _1305;
    always @(posedge _84) begin
        if (_82)
            _1275 <= _1274;
        else
            _1275 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _1278 <= _1277;
        else
            _1278 <= _1275;
    end
    always @(posedge _84) begin
        if (_82)
            _1281 <= _1280;
        else
            _1281 <= _1278;
    end
    always @(posedge _84) begin
        if (_82)
            _1284 <= _1283;
        else
            _1284 <= _1281;
    end
    always @(posedge _84) begin
        if (_82)
            _1287 <= _1286;
        else
            _1287 <= _1284;
    end
    always @(posedge _84) begin
        if (_82)
            _1290 <= _1289;
        else
            _1290 <= _1287;
    end
    always @(posedge _84) begin
        if (_82)
            _1293 <= _1292;
        else
            _1293 <= _1290;
    end
    always @(posedge _84) begin
        if (_82)
            _1296 <= _1295;
        else
            _1296 <= _1293;
    end
    always @(posedge _84) begin
        if (_82)
            _1299 <= _1298;
        else
            _1299 <= _1296;
    end
    assign _1300 = ~ _1299;
    always @(posedge _84) begin
        if (_82)
            _1248 <= _1247;
        else
            _1248 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _1251 <= _1250;
        else
            _1251 <= _1248;
    end
    always @(posedge _84) begin
        if (_82)
            _1254 <= _1253;
        else
            _1254 <= _1251;
    end
    always @(posedge _84) begin
        if (_82)
            _1257 <= _1256;
        else
            _1257 <= _1254;
    end
    always @(posedge _84) begin
        if (_82)
            _1260 <= _1259;
        else
            _1260 <= _1257;
    end
    always @(posedge _84) begin
        if (_82)
            _1263 <= _1262;
        else
            _1263 <= _1260;
    end
    always @(posedge _84) begin
        if (_82)
            _1266 <= _1265;
        else
            _1266 <= _1263;
    end
    always @(posedge _84) begin
        if (_82)
            _1269 <= _1268;
        else
            _1269 <= _1266;
    end
    always @(posedge _84) begin
        if (_82)
            _1272 <= _1271;
        else
            _1272 <= _1269;
    end
    assign _1301 = _1272 & _1300;
    assign _1302 = _129 & _1301;
    assign write_enable_70 = _1302 & PHASE_16;
    assign _1307 = write_enable_70 | read_enable_58;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_34
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1307), .regcea(vdd), .wea(write_enable_70), .addra(address_70), .dina(data_67), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_61), .regceb(vdd), .web(write_enable_61), .addrb(address_61), .dinb(data_63), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1314[131:131]), .sbiterrb(_1314[130:130]), .doutb(_1314[129:66]), .dbiterra(_1314[65:65]), .sbiterra(_1314[64:64]), .douta(_1314[63:0]) );
    assign _1315 = _1314[63:0];
    assign _1245 = ~ PHASE_16;
    assign _47 = _1245;
    always @(posedge _84) begin
        if (_82)
            PHASE_16 <= _1243;
        else
            if (_72)
                PHASE_16 <= _47;
    end
    assign _1327 = PHASE_16 ? _1326 : _1315;
    assign address_71 = _1506 ? _199 : _1501;
    assign write_enable_71 = _1499 & _1506;
    assign address_72 = _1506 ? _164 : _68;
    assign _1508 = ~ _1506;
    assign read_enable_59 = _1494 & _1508;
    assign _1506 = ~ PHASE_19;
    assign write_enable_72 = _1492 & _1506;
    assign _1510 = write_enable_72 | read_enable_59;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_35
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1510), .regcea(vdd), .wea(write_enable_72), .addra(address_72), .dina(data_79), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_71), .regceb(vdd), .web(write_enable_71), .addrb(address_71), .dinb(data_75), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1515[131:131]), .sbiterrb(_1515[130:130]), .doutb(_1515[129:66]), .dbiterra(_1515[65:65]), .sbiterra(_1515[64:64]), .douta(_1515[63:0]) );
    assign _1516 = _1515[63:0];
    assign address_73 = PHASE_19 ? _199 : _1501;
    assign _1499 = _129 & _1462;
    assign write_enable_73 = _1499 & PHASE_19;
    assign _1424 = _1416[129:66];
    assign _1423 = _1403[129:66];
    assign _1425 = PHASE_17 ? _1424 : _1423;
    assign _1421 = _1367[129:66];
    assign _1420 = _1353[129:66];
    assign q1_5 = PHASE_18 ? _1421 : _1420;
    assign _1426 = _1334 ? _1425 : q1_5;
    assign address_74 = _1405 ? _1399 : _1398;
    assign _1411 = ~ _1405;
    assign read_enable_60 = _102 & _1411;
    assign address_75 = _1405 ? _60 : _1384;
    assign _1407 = ~ _1405;
    assign read_enable_61 = _102 & _1407;
    assign _1405 = ~ PHASE_17;
    assign write_enable_75 = _1370 & _1405;
    assign _1409 = write_enable_75 | read_enable_61;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_36
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1409), .regcea(vdd), .wea(write_enable_75), .addra(address_75), .dina(data_73), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_60), .regceb(vdd), .web(write_enable_74), .addrb(address_74), .dinb(data_71), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1416[131:131]), .sbiterrb(_1416[130:130]), .doutb(_1416[129:66]), .dbiterra(_1416[65:65]), .sbiterra(_1416[64:64]), .douta(_1416[63:0]) );
    assign _1417 = _1416[63:0];
    assign _1397 = _172[8:8];
    assign _1396 = _172[7:7];
    assign _1395 = _172[6:6];
    assign _1394 = _172[5:5];
    assign _1393 = _172[4:4];
    assign _1392 = _172[3:3];
    assign _1391 = _172[2:2];
    assign _1390 = _172[1:1];
    assign _1389 = _172[0:0];
    assign _1398 = { _1389, _1390, _1391, _1392, _1393, _1394, _1395, _1396, _1397 };
    assign address_76 = PHASE_17 ? _1399 : _1398;
    assign _1386 = ~ PHASE_17;
    assign read_enable_62 = _102 & _1386;
    assign data_73 = wr_d1;
    assign _1383 = _137[8:8];
    assign _1382 = _137[7:7];
    assign _1381 = _137[6:6];
    assign _1380 = _137[5:5];
    assign _1379 = _137[4:4];
    assign _1378 = _137[3:3];
    assign _1377 = _137[2:2];
    assign _1376 = _137[1:1];
    assign _1375 = _137[0:0];
    assign _1384 = { _1375, _1376, _1377, _1378, _1379, _1380, _1381, _1382, _1383 };
    assign address_77 = PHASE_17 ? _60 : _1384;
    assign _1372 = ~ PHASE_17;
    assign read_enable_63 = _102 & _1372;
    assign _1370 = _62[1:1];
    assign write_enable_77 = _1370 & PHASE_17;
    assign _1374 = write_enable_77 | read_enable_63;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_37
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1374), .regcea(vdd), .wea(write_enable_77), .addra(address_77), .dina(data_73), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_62), .regceb(vdd), .web(write_enable_76), .addrb(address_76), .dinb(data_71), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1403[131:131]), .sbiterrb(_1403[130:130]), .doutb(_1403[129:66]), .dbiterra(_1403[65:65]), .sbiterra(_1403[64:64]), .douta(_1403[63:0]) );
    assign _1404 = _1403[63:0];
    assign _1331 = ~ PHASE_17;
    assign _51 = _1331;
    always @(posedge _84) begin
        if (_82)
            PHASE_17 <= _1329;
        else
            if (_72)
                PHASE_17 <= _51;
    end
    assign _1418 = PHASE_17 ? _1417 : _1404;
    assign address_78 = _1355 ? _199 : _172;
    assign _1362 = ~ _1355;
    assign read_enable_64 = _102 & _1362;
    assign write_enable_78 = _1346 & _1355;
    assign _1364 = write_enable_78 | read_enable_64;
    assign address_79 = _1355 ? _164 : _137;
    assign _1357 = ~ _1355;
    assign read_enable_65 = _102 & _1357;
    assign _1355 = ~ PHASE_18;
    assign write_enable_79 = _1339 & _1355;
    assign _1359 = write_enable_79 | read_enable_65;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_38
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1359), .regcea(vdd), .wea(write_enable_79), .addra(address_79), .dina(data_79), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_1364), .regceb(vdd), .web(write_enable_78), .addrb(address_78), .dinb(data_75), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1367[131:131]), .sbiterrb(_1367[130:130]), .doutb(_1367[129:66]), .dbiterra(_1367[65:65]), .sbiterra(_1367[64:64]), .douta(_1367[63:0]) );
    assign _1368 = _1367[63:0];
    assign _1429 = _1428[127:64];
    assign data_75 = _1429;
    assign address_80 = PHASE_18 ? _199 : _172;
    assign _1348 = ~ PHASE_18;
    assign read_enable_66 = _102 & _1348;
    assign _1345 = ~ _130;
    assign _1346 = _129 & _1345;
    assign write_enable_80 = _1346 & PHASE_18;
    assign _1350 = write_enable_80 | read_enable_66;
    assign address_81 = PHASE_18 ? _164 : _137;
    assign _1341 = ~ PHASE_18;
    assign read_enable_67 = _102 & _1341;
    assign _1338 = ~ _130;
    assign _1339 = _129 & _1338;
    assign write_enable_81 = _1339 & PHASE_18;
    assign _1343 = write_enable_81 | read_enable_67;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_39
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1343), .regcea(vdd), .wea(write_enable_81), .addra(address_81), .dina(data_79), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_1350), .regceb(vdd), .web(write_enable_80), .addrb(address_80), .dinb(data_75), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1353[131:131]), .sbiterrb(_1353[130:130]), .doutb(_1353[129:66]), .dbiterra(_1353[65:65]), .sbiterra(_1353[64:64]), .douta(_1353[63:0]) );
    assign _1354 = _1353[63:0];
    assign _1430 = ~ PHASE_18;
    assign _53 = _1430;
    always @(posedge _84) begin
        if (_82)
            PHASE_18 <= _1336;
        else
            if (_99)
                PHASE_18 <= _53;
    end
    assign q0_5 = PHASE_18 ? _1368 : _1354;
    always @(posedge _84) begin
        if (_82)
            _1334 <= _1333;
        else
            _1334 <= _92;
    end
    assign _1419 = _1334 ? _1418 : q0_5;
    dp_0
        dp_0
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_1419), .d2(_1426), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_1428[191:128]), .q2(_1428[127:64]), .q1(_1428[63:0]) );
    assign _1431 = _1428[63:0];
    assign data_79 = _1431;
    assign address_82 = PHASE_19 ? _164 : _68;
    assign _1495 = ~ PHASE_19;
    assign _1494 = _70[1:1];
    assign read_enable_68 = _1494 & _1495;
    always @(posedge _84) begin
        if (_82)
            _1465 <= _1464;
        else
            _1465 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _1468 <= _1467;
        else
            _1468 <= _1465;
    end
    always @(posedge _84) begin
        if (_82)
            _1471 <= _1470;
        else
            _1471 <= _1468;
    end
    always @(posedge _84) begin
        if (_82)
            _1474 <= _1473;
        else
            _1474 <= _1471;
    end
    always @(posedge _84) begin
        if (_82)
            _1477 <= _1476;
        else
            _1477 <= _1474;
    end
    always @(posedge _84) begin
        if (_82)
            _1480 <= _1479;
        else
            _1480 <= _1477;
    end
    always @(posedge _84) begin
        if (_82)
            _1483 <= _1482;
        else
            _1483 <= _1480;
    end
    always @(posedge _84) begin
        if (_82)
            _1486 <= _1485;
        else
            _1486 <= _1483;
    end
    always @(posedge _84) begin
        if (_82)
            _1489 <= _1488;
        else
            _1489 <= _1486;
    end
    assign _1490 = ~ _1489;
    always @(posedge _84) begin
        if (_82)
            _1438 <= _1437;
        else
            _1438 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _1441 <= _1440;
        else
            _1441 <= _1438;
    end
    always @(posedge _84) begin
        if (_82)
            _1444 <= _1443;
        else
            _1444 <= _1441;
    end
    always @(posedge _84) begin
        if (_82)
            _1447 <= _1446;
        else
            _1447 <= _1444;
    end
    always @(posedge _84) begin
        if (_82)
            _1450 <= _1449;
        else
            _1450 <= _1447;
    end
    always @(posedge _84) begin
        if (_82)
            _1453 <= _1452;
        else
            _1453 <= _1450;
    end
    always @(posedge _84) begin
        if (_82)
            _1456 <= _1455;
        else
            _1456 <= _1453;
    end
    always @(posedge _84) begin
        if (_82)
            _1459 <= _1458;
        else
            _1459 <= _1456;
    end
    always @(posedge _84) begin
        if (_82)
            _1462 <= _1461;
        else
            _1462 <= _1459;
    end
    assign _1491 = _1462 & _1490;
    assign _1492 = _129 & _1491;
    assign write_enable_82 = _1492 & PHASE_19;
    assign _1497 = write_enable_82 | read_enable_68;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_40
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1497), .regcea(vdd), .wea(write_enable_82), .addra(address_82), .dina(data_79), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_73), .regceb(vdd), .web(write_enable_73), .addrb(address_73), .dinb(data_75), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1504[131:131]), .sbiterrb(_1504[130:130]), .doutb(_1504[129:66]), .dbiterra(_1504[65:65]), .sbiterra(_1504[64:64]), .douta(_1504[63:0]) );
    assign _1505 = _1504[63:0];
    assign _1435 = ~ PHASE_19;
    assign _55 = _1435;
    always @(posedge _84) begin
        if (_82)
            PHASE_19 <= _1433;
        else
            if (_72)
                PHASE_19 <= _55;
    end
    assign _1517 = PHASE_19 ? _1516 : _1505;
    assign address_83 = _1696 ? _199 : _1691;
    assign write_enable_83 = _1689 & _1696;
    assign address_84 = _1696 ? _164 : _68;
    assign _1698 = ~ _1696;
    assign read_enable_69 = _1684 & _1698;
    assign _1696 = ~ PHASE_22;
    assign write_enable_84 = _1682 & _1696;
    assign _1700 = write_enable_84 | read_enable_69;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_41
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1700), .regcea(vdd), .wea(write_enable_84), .addra(address_84), .dina(data_91), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_83), .regceb(vdd), .web(write_enable_83), .addrb(address_83), .dinb(data_87), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1705[131:131]), .sbiterrb(_1705[130:130]), .doutb(_1705[129:66]), .dbiterra(_1705[65:65]), .sbiterra(_1705[64:64]), .douta(_1705[63:0]) );
    assign _1706 = _1705[63:0];
    assign address_85 = PHASE_22 ? _199 : _1691;
    assign _1689 = _129 & _1652;
    assign write_enable_85 = _1689 & PHASE_22;
    assign _286 = _91[506:503];
    assign _285 = _91[502:502];
    assign _283 = _91[498:498];
    assign _282 = _91[497:434];
    assign _281 = _91[433:370];
    assign _280 = _91[369:306];
    assign _279 = _91[305:242];
    assign _278 = _91[241:178];
    assign _277 = _91[177:114];
    assign _276 = _91[113:50];
    assign _1614 = _1606[129:66];
    assign _1613 = _1593[129:66];
    assign _1615 = PHASE_20 ? _1614 : _1613;
    assign _1611 = _1557[129:66];
    assign _1610 = _1543[129:66];
    assign q1_6 = PHASE_21 ? _1611 : _1610;
    assign _1616 = _1524 ? _1615 : q1_6;
    assign address_86 = _1595 ? _1589 : _1588;
    assign _1601 = ~ _1595;
    assign read_enable_70 = _102 & _1601;
    assign address_87 = _1595 ? _60 : _1574;
    assign _1597 = ~ _1595;
    assign read_enable_71 = _102 & _1597;
    assign _1595 = ~ PHASE_20;
    assign write_enable_87 = _1560 & _1595;
    assign _1599 = write_enable_87 | read_enable_71;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_42
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1599), .regcea(vdd), .wea(write_enable_87), .addra(address_87), .dina(data_85), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_70), .regceb(vdd), .web(write_enable_86), .addrb(address_86), .dinb(data_83), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1606[131:131]), .sbiterrb(_1606[130:130]), .doutb(_1606[129:66]), .dbiterra(_1606[65:65]), .sbiterra(_1606[64:64]), .douta(_1606[63:0]) );
    assign _1607 = _1606[63:0];
    assign _1587 = _172[8:8];
    assign _1586 = _172[7:7];
    assign _1585 = _172[6:6];
    assign _1584 = _172[5:5];
    assign _1583 = _172[4:4];
    assign _1582 = _172[3:3];
    assign _1581 = _172[2:2];
    assign _1580 = _172[1:1];
    assign _1579 = _172[0:0];
    assign _1588 = { _1579, _1580, _1581, _1582, _1583, _1584, _1585, _1586, _1587 };
    assign address_88 = PHASE_20 ? _1589 : _1588;
    assign _1576 = ~ PHASE_20;
    assign read_enable_72 = _102 & _1576;
    assign data_85 = wr_d0;
    assign _60 = wr_addr;
    assign _1573 = _137[8:8];
    assign _1572 = _137[7:7];
    assign _1571 = _137[6:6];
    assign _1570 = _137[5:5];
    assign _1569 = _137[4:4];
    assign _1568 = _137[3:3];
    assign _1567 = _137[2:2];
    assign _1566 = _137[1:1];
    assign _1565 = _137[0:0];
    assign _1574 = { _1565, _1566, _1567, _1568, _1569, _1570, _1571, _1572, _1573 };
    assign address_89 = PHASE_20 ? _60 : _1574;
    assign _1562 = ~ PHASE_20;
    assign read_enable_73 = _102 & _1562;
    assign _62 = wr_en;
    assign _1560 = _62[0:0];
    assign write_enable_89 = _1560 & PHASE_20;
    assign _1564 = write_enable_89 | read_enable_73;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_43
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1564), .regcea(vdd), .wea(write_enable_89), .addra(address_89), .dina(data_85), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(read_enable_72), .regceb(vdd), .web(write_enable_88), .addrb(address_88), .dinb(data_83), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1593[131:131]), .sbiterrb(_1593[130:130]), .doutb(_1593[129:66]), .dbiterra(_1593[65:65]), .sbiterra(_1593[64:64]), .douta(_1593[63:0]) );
    assign _1594 = _1593[63:0];
    assign _1521 = ~ PHASE_20;
    assign _63 = _1521;
    always @(posedge _84) begin
        if (_82)
            PHASE_20 <= _1519;
        else
            if (_72)
                PHASE_20 <= _63;
    end
    assign _1608 = PHASE_20 ? _1607 : _1594;
    assign address_90 = _1545 ? _199 : _172;
    assign _1552 = ~ _1545;
    assign read_enable_74 = _102 & _1552;
    assign write_enable_90 = _1536 & _1545;
    assign _1554 = write_enable_90 | read_enable_74;
    assign address_91 = _1545 ? _164 : _137;
    assign _1547 = ~ _1545;
    assign read_enable_75 = _102 & _1547;
    assign _1545 = ~ PHASE_21;
    assign write_enable_91 = _1529 & _1545;
    assign _1549 = write_enable_91 | read_enable_75;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_44
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1549), .regcea(vdd), .wea(write_enable_91), .addra(address_91), .dina(data_91), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_1554), .regceb(vdd), .web(write_enable_90), .addrb(address_90), .dinb(data_87), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1557[131:131]), .sbiterrb(_1557[130:130]), .doutb(_1557[129:66]), .dbiterra(_1557[65:65]), .sbiterra(_1557[64:64]), .douta(_1557[63:0]) );
    assign _1558 = _1557[63:0];
    assign _1619 = _1618[127:64];
    assign data_87 = _1619;
    always @(posedge _84) begin
        if (_82)
            _175 <= _174;
        else
            _175 <= _172;
    end
    always @(posedge _84) begin
        if (_82)
            _178 <= _177;
        else
            _178 <= _175;
    end
    always @(posedge _84) begin
        if (_82)
            _181 <= _180;
        else
            _181 <= _178;
    end
    always @(posedge _84) begin
        if (_82)
            _184 <= _183;
        else
            _184 <= _181;
    end
    always @(posedge _84) begin
        if (_82)
            _187 <= _186;
        else
            _187 <= _184;
    end
    always @(posedge _84) begin
        if (_82)
            _190 <= _189;
        else
            _190 <= _187;
    end
    always @(posedge _84) begin
        if (_82)
            _193 <= _192;
        else
            _193 <= _190;
    end
    always @(posedge _84) begin
        if (_82)
            _196 <= _195;
        else
            _196 <= _193;
    end
    always @(posedge _84) begin
        if (_82)
            _199 <= _198;
        else
            _199 <= _196;
    end
    assign _172 = _91[49:41];
    assign address_92 = PHASE_21 ? _199 : _172;
    assign _1538 = ~ PHASE_21;
    assign read_enable_76 = _102 & _1538;
    assign _1535 = ~ _130;
    assign _1536 = _129 & _1535;
    assign write_enable_92 = _1536 & PHASE_21;
    assign _1540 = write_enable_92 | read_enable_76;
    assign address_93 = PHASE_21 ? _164 : _137;
    assign _1531 = ~ PHASE_21;
    assign read_enable_77 = _102 & _1531;
    assign _1528 = ~ _130;
    assign _1529 = _129 & _1528;
    assign write_enable_93 = _1529 & PHASE_21;
    assign _1533 = write_enable_93 | read_enable_77;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_45
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1533), .regcea(vdd), .wea(write_enable_93), .addra(address_93), .dina(data_91), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(_1540), .regceb(vdd), .web(write_enable_92), .addrb(address_92), .dinb(data_87), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1543[131:131]), .sbiterrb(_1543[130:130]), .doutb(_1543[129:66]), .dbiterra(_1543[65:65]), .sbiterra(_1543[64:64]), .douta(_1543[63:0]) );
    assign _1544 = _1543[63:0];
    assign _99 = _91[508:508];
    assign _1620 = ~ PHASE_21;
    assign _65 = _1620;
    always @(posedge _84) begin
        if (_82)
            PHASE_21 <= _1526;
        else
            if (_99)
                PHASE_21 <= _65;
    end
    assign q0_6 = PHASE_21 ? _1558 : _1544;
    assign _92 = _91[499:499];
    always @(posedge _84) begin
        if (_82)
            _1524 <= _1523;
        else
            _1524 <= _92;
    end
    assign _1609 = _1524 ? _1608 : q0_6;
    dp
        dp
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .d1(_1609), .d2(_1616), .omegas0(_276), .omegas1(_277), .omegas2(_278), .omegas3(_279), .omegas4(_280), .omegas5(_281), .omegas6(_282), .start_twiddles(_283), .twiddle_stage(_284), .valid(_285), .index(_286), .twiddle_update_q(_1618[191:128]), .q2(_1618[127:64]), .q1(_1618[63:0]) );
    assign _1621 = _1618[63:0];
    assign data_91 = _1621;
    assign _137 = _91[40:32];
    always @(posedge _84) begin
        if (_82)
            _140 <= _139;
        else
            _140 <= _137;
    end
    always @(posedge _84) begin
        if (_82)
            _143 <= _142;
        else
            _143 <= _140;
    end
    always @(posedge _84) begin
        if (_82)
            _146 <= _145;
        else
            _146 <= _143;
    end
    always @(posedge _84) begin
        if (_82)
            _149 <= _148;
        else
            _149 <= _146;
    end
    always @(posedge _84) begin
        if (_82)
            _152 <= _151;
        else
            _152 <= _149;
    end
    always @(posedge _84) begin
        if (_82)
            _155 <= _154;
        else
            _155 <= _152;
    end
    always @(posedge _84) begin
        if (_82)
            _158 <= _157;
        else
            _158 <= _155;
    end
    always @(posedge _84) begin
        if (_82)
            _161 <= _160;
        else
            _161 <= _158;
    end
    always @(posedge _84) begin
        if (_82)
            _164 <= _163;
        else
            _164 <= _161;
    end
    assign _68 = rd_addr;
    assign address_94 = PHASE_22 ? _164 : _68;
    assign _1685 = ~ PHASE_22;
    assign _70 = rd_en;
    assign _1684 = _70[0:0];
    assign read_enable_78 = _1684 & _1685;
    assign _284 = _91[501:501];
    always @(posedge _84) begin
        if (_82)
            _1655 <= _1654;
        else
            _1655 <= _284;
    end
    always @(posedge _84) begin
        if (_82)
            _1658 <= _1657;
        else
            _1658 <= _1655;
    end
    always @(posedge _84) begin
        if (_82)
            _1661 <= _1660;
        else
            _1661 <= _1658;
    end
    always @(posedge _84) begin
        if (_82)
            _1664 <= _1663;
        else
            _1664 <= _1661;
    end
    always @(posedge _84) begin
        if (_82)
            _1667 <= _1666;
        else
            _1667 <= _1664;
    end
    always @(posedge _84) begin
        if (_82)
            _1670 <= _1669;
        else
            _1670 <= _1667;
    end
    always @(posedge _84) begin
        if (_82)
            _1673 <= _1672;
        else
            _1673 <= _1670;
    end
    always @(posedge _84) begin
        if (_82)
            _1676 <= _1675;
        else
            _1676 <= _1673;
    end
    always @(posedge _84) begin
        if (_82)
            _1679 <= _1678;
        else
            _1679 <= _1676;
    end
    assign _1680 = ~ _1679;
    assign _130 = _91[500:500];
    always @(posedge _84) begin
        if (_82)
            _1628 <= _1627;
        else
            _1628 <= _130;
    end
    always @(posedge _84) begin
        if (_82)
            _1631 <= _1630;
        else
            _1631 <= _1628;
    end
    always @(posedge _84) begin
        if (_82)
            _1634 <= _1633;
        else
            _1634 <= _1631;
    end
    always @(posedge _84) begin
        if (_82)
            _1637 <= _1636;
        else
            _1637 <= _1634;
    end
    always @(posedge _84) begin
        if (_82)
            _1640 <= _1639;
        else
            _1640 <= _1637;
    end
    always @(posedge _84) begin
        if (_82)
            _1643 <= _1642;
        else
            _1643 <= _1640;
    end
    always @(posedge _84) begin
        if (_82)
            _1646 <= _1645;
        else
            _1646 <= _1643;
    end
    always @(posedge _84) begin
        if (_82)
            _1649 <= _1648;
        else
            _1649 <= _1646;
    end
    always @(posedge _84) begin
        if (_82)
            _1652 <= _1651;
        else
            _1652 <= _1649;
    end
    assign _1681 = _1652 & _1680;
    assign _102 = _91[507:507];
    always @(posedge _84) begin
        if (_82)
            _105 <= _104;
        else
            _105 <= _102;
    end
    always @(posedge _84) begin
        if (_82)
            _108 <= _107;
        else
            _108 <= _105;
    end
    always @(posedge _84) begin
        if (_82)
            _111 <= _110;
        else
            _111 <= _108;
    end
    always @(posedge _84) begin
        if (_82)
            _114 <= _113;
        else
            _114 <= _111;
    end
    always @(posedge _84) begin
        if (_82)
            _117 <= _116;
        else
            _117 <= _114;
    end
    always @(posedge _84) begin
        if (_82)
            _120 <= _119;
        else
            _120 <= _117;
    end
    always @(posedge _84) begin
        if (_82)
            _123 <= _122;
        else
            _123 <= _120;
    end
    always @(posedge _84) begin
        if (_82)
            _126 <= _125;
        else
            _126 <= _123;
    end
    always @(posedge _84) begin
        if (_82)
            _129 <= _128;
        else
            _129 <= _126;
    end
    assign _1682 = _129 & _1681;
    assign write_enable_94 = _1682 & PHASE_22;
    assign _1687 = write_enable_94 | read_enable_78;
    xpm_memory_tdpram
        #( .MEMORY_SIZE(32768), .MEMORY_PRIMITIVE("ultra"), .CLOCKING_MODE("common_clock"), .ECC_MODE("no_ecc"), .MEMORY_INIT_FILE("none"), .MEMORY_INIT_PARAM(""), .USE_MEM_INIT(0), .WAKEUP_TIME("disable_sleep"), .AUTO_SLEEP_TIME(0), .MESSAGE_CONTROL(0), .USE_EMBEDDED_CONSTRAINT(0), .MEMORY_OPTIMIZATION("true"), .CASCADE_HEIGHT(0), .SIM_ASSERT_CHK(0), .WRITE_DATA_WIDTH_A(64), .READ_DATA_WIDTH_A(64), .BYTE_WRITE_WIDTH_A(64), .ADDR_WIDTH_A(9), .READ_RESET_VALUE_A("0"), .READ_LATENCY_A(1), .WRITE_MODE_A("no_change"), .RST_MODE_A("SYNC"), .WRITE_DATA_WIDTH_B(64), .READ_DATA_WIDTH_B(64), .BYTE_WRITE_WIDTH_B(64), .ADDR_WIDTH_B(9), .READ_RESET_VALUE_B("0"), .READ_LATENCY_B(1), .WRITE_MODE_B("no_change"), .RST_MODE_B("SYNC") )
        the_xpm_memory_tdpram_46
        ( .sleep(gnd), .clka(_84), .rsta(gnd), .ena(_1687), .regcea(vdd), .wea(write_enable_94), .addra(address_94), .dina(data_91), .injectsbiterra(gnd), .injectdbiterra(gnd), .clkb(_84), .rstb(gnd), .enb(write_enable_85), .regceb(vdd), .web(write_enable_85), .addrb(address_85), .dinb(data_87), .injectsbiterrb(gnd), .injectdbiterrb(gnd), .dbiterrb(_1694[131:131]), .sbiterrb(_1694[130:130]), .doutb(_1694[129:66]), .dbiterra(_1694[65:65]), .sbiterra(_1694[64:64]), .douta(_1694[63:0]) );
    assign _1695 = _1694[63:0];
    assign _72 = flip;
    assign _1625 = ~ PHASE_22;
    assign _73 = _1625;
    always @(posedge _84) begin
        if (_82)
            PHASE_22 <= _1623;
        else
            if (_72)
                PHASE_22 <= _73;
    end
    assign _1707 = PHASE_22 ? _1706 : _1695;
    assign _76 = first_4step_pass;
    assign _78 = first_iter;
    assign _80 = start;
    assign _82 = clear;
    assign _84 = clock;
    ctrl
        ctrl
        ( .clock(_84), .clear(_82), .start(_80), .first_iter(_78), .first_4step_pass(_76), .flip(_91[508:508]), .read_write_enable(_91[507:507]), .index(_91[506:503]), .valid(_91[502:502]), .twiddle_stage(_91[501:501]), .last_stage(_91[500:500]), .first_stage(_91[499:499]), .start_twiddles(_91[498:498]), .omegas6(_91[497:434]), .omegas5(_91[433:370]), .omegas4(_91[369:306]), .omegas3(_91[305:242]), .omegas2(_91[241:178]), .omegas1(_91[177:114]), .omegas0(_91[113:50]), .addr2(_91[49:41]), .addr1(_91[40:32]), .m(_91[31:23]), .k(_91[22:14]), .j(_91[13:5]), .i(_91[4:1]), .done_(_91[0:0]) );
    assign _1708 = _91[0:0];

    /* aliases */
    assign data_0 = data;
    assign data_2 = data_1;
    assign data_4 = data_3;
    assign data_5 = data_3;
    assign data_6 = data_3;
    assign data_8 = data_7;
    assign data_9 = data_7;
    assign data_10 = data_7;
    assign data_12 = data_11;
    assign data_14 = data_13;
    assign data_16 = data_15;
    assign data_17 = data_15;
    assign data_18 = data_15;
    assign data_20 = data_19;
    assign data_21 = data_19;
    assign data_22 = data_19;
    assign data_24 = data_23;
    assign data_26 = data_25;
    assign data_28 = data_27;
    assign data_29 = data_27;
    assign data_30 = data_27;
    assign data_32 = data_31;
    assign data_33 = data_31;
    assign data_34 = data_31;
    assign data_36 = data_35;
    assign data_38 = data_37;
    assign data_40 = data_39;
    assign data_41 = data_39;
    assign data_42 = data_39;
    assign data_44 = data_43;
    assign data_45 = data_43;
    assign data_46 = data_43;
    assign data_48 = data_47;
    assign data_50 = data_49;
    assign data_52 = data_51;
    assign data_53 = data_51;
    assign data_54 = data_51;
    assign data_56 = data_55;
    assign data_57 = data_55;
    assign data_58 = data_55;
    assign data_60 = data_59;
    assign data_62 = data_61;
    assign data_64 = data_63;
    assign data_65 = data_63;
    assign data_66 = data_63;
    assign data_68 = data_67;
    assign data_69 = data_67;
    assign data_70 = data_67;
    assign data_72 = data_71;
    assign data_74 = data_73;
    assign data_76 = data_75;
    assign data_77 = data_75;
    assign data_78 = data_75;
    assign data_80 = data_79;
    assign data_81 = data_79;
    assign data_82 = data_79;
    assign data_84 = data_83;
    assign data_86 = data_85;
    assign data_88 = data_87;
    assign data_89 = data_87;
    assign data_90 = data_87;
    assign data_92 = data_91;
    assign data_93 = data_91;
    assign data_94 = data_91;

    /* output assignments */
    assign done_ = _1708;
    assign rd_q0 = _1707;
    assign rd_q1 = _1517;
    assign rd_q2 = _1327;
    assign rd_q3 = _1137;
    assign rd_q4 = _947;
    assign rd_q5 = _757;
    assign rd_q6 = _567;
    assign rd_q7 = _377;

endmodule
module multi_parallel_cores (
    rd_addr0,
    rd_en,
    wr_addr0,
    wr_en,
    wr_d_0_7,
    wr_d_0_6,
    wr_d_0_5,
    wr_d_0_4,
    wr_d_0_3,
    wr_d_0_2,
    wr_d_0_1,
    wr_d_0_0,
    flip,
    first_iter,
    first_4step_pass,
    start,
    clear,
    clock,
    done_,
    rd_d_0_0,
    rd_d_0_1,
    rd_d_0_2,
    rd_d_0_3,
    rd_d_0_4,
    rd_d_0_5,
    rd_d_0_6,
    rd_d_0_7
);

    input [8:0] rd_addr0;
    input rd_en;
    input [8:0] wr_addr0;
    input wr_en;
    input [63:0] wr_d_0_7;
    input [63:0] wr_d_0_6;
    input [63:0] wr_d_0_5;
    input [63:0] wr_d_0_4;
    input [63:0] wr_d_0_3;
    input [63:0] wr_d_0_2;
    input [63:0] wr_d_0_1;
    input [63:0] wr_d_0_0;
    input flip;
    input first_iter;
    input first_4step_pass;
    input start;
    input clear;
    input clock;
    output done_;
    output [63:0] rd_d_0_0;
    output [63:0] rd_d_0_1;
    output [63:0] rd_d_0_2;
    output [63:0] rd_d_0_3;
    output [63:0] rd_d_0_4;
    output [63:0] rd_d_0_5;
    output [63:0] rd_d_0_6;
    output [63:0] rd_d_0_7;

    /* signal declarations */
    wire [63:0] _54;
    wire [63:0] _55;
    wire [63:0] _56;
    wire [63:0] _57;
    wire [63:0] _58;
    wire [63:0] _59;
    wire [63:0] _60;
    wire [63:0] _61;
    wire [8:0] _10;
    wire _12;
    wire [1:0] _49;
    wire [3:0] _50;
    wire [7:0] _51;
    wire [8:0] _14;
    wire _16;
    wire [1:0] _46;
    wire [3:0] _47;
    wire [7:0] _48;
    wire [63:0] _18;
    wire [63:0] _20;
    wire [63:0] _22;
    wire [63:0] _24;
    wire [63:0] _26;
    wire [63:0] _28;
    wire [63:0] _30;
    wire [63:0] _32;
    wire _34;
    wire _36;
    wire _38;
    wire _40;
    wire _42;
    wire _44;
    wire [512:0] _53;
    wire _62;

    /* logic */
    assign _54 = _53[512:449];
    assign _55 = _53[448:385];
    assign _56 = _53[384:321];
    assign _57 = _53[320:257];
    assign _58 = _53[256:193];
    assign _59 = _53[192:129];
    assign _60 = _53[128:65];
    assign _61 = _53[64:1];
    assign _10 = rd_addr0;
    assign _12 = rd_en;
    assign _49 = { _12, _12 };
    assign _50 = { _49, _49 };
    assign _51 = { _50, _50 };
    assign _14 = wr_addr0;
    assign _16 = wr_en;
    assign _46 = { _16, _16 };
    assign _47 = { _46, _46 };
    assign _48 = { _47, _47 };
    assign _18 = wr_d_0_7;
    assign _20 = wr_d_0_6;
    assign _22 = wr_d_0_5;
    assign _24 = wr_d_0_4;
    assign _26 = wr_d_0_3;
    assign _28 = wr_d_0_2;
    assign _30 = wr_d_0_1;
    assign _32 = wr_d_0_0;
    assign _34 = flip;
    assign _36 = first_iter;
    assign _38 = first_4step_pass;
    assign _40 = start;
    assign _42 = clear;
    assign _44 = clock;
    parallel_cores
        parallel_cores
        ( .clock(_44), .clear(_42), .start(_40), .first_4step_pass(_38), .first_iter(_36), .flip(_34), .wr_d0(_32), .wr_d1(_30), .wr_d2(_28), .wr_d3(_26), .wr_d4(_24), .wr_d5(_22), .wr_d6(_20), .wr_d7(_18), .wr_en(_48), .wr_addr(_14), .rd_en(_51), .rd_addr(_10), .rd_q7(_53[512:449]), .rd_q6(_53[448:385]), .rd_q5(_53[384:321]), .rd_q4(_53[320:257]), .rd_q3(_53[256:193]), .rd_q2(_53[192:129]), .rd_q1(_53[128:65]), .rd_q0(_53[64:1]), .done_(_53[0:0]) );
    assign _62 = _53[0:0];

    /* aliases */

    /* output assignments */
    assign done_ = _62;
    assign rd_d_0_0 = _61;
    assign rd_d_0_1 = _60;
    assign rd_d_0_2 = _59;
    assign rd_d_0_3 = _58;
    assign rd_d_0_4 = _57;
    assign rd_d_0_5 = _56;
    assign rd_d_0_6 = _55;
    assign rd_d_0_7 = _54;

endmodule
module kernel (
    data_in_tdata,
    data_in_tvalid,
    first_4step_pass,
    start,
    data_out_dest_tready,
    clear,
    clock,
    data_in_tkeep,
    data_in_tlast,
    data_in_tstrb,
    data_out_tvalid,
    data_out_tdata,
    data_out_tkeep,
    data_out_tstrb,
    data_out_tlast,
    data_in_dest_tready,
    done_
);

    input [511:0] data_in_tdata;
    input data_in_tvalid;
    input first_4step_pass;
    input start;
    input data_out_dest_tready;
    input clear;
    input clock;
    input [63:0] data_in_tkeep;
    input data_in_tlast;
    input [63:0] data_in_tstrb;
    output data_out_tvalid;
    output [511:0] data_out_tdata;
    output [63:0] data_out_tkeep;
    output [63:0] data_out_tstrb;
    output data_out_tlast;
    output data_in_dest_tready;
    output done_;

    /* signal declarations */
    wire _33;
    wire _34;
    wire gnd = 1'b0;
    wire [63:0] _36 = 64'b1111111111111111111111111111111111111111111111111111111111111111;
    wire [63:0] _37 = 64'b1111111111111111111111111111111111111111111111111111111111111111;
    wire [511:0] _123 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _122 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _120 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _119 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _117 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _116 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _114 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _113 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire _109 = 1'b0;
    wire _110;
    wire _111;
    wire [511:0] _108 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _107 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [63:0] _105;
    wire [63:0] _104;
    wire [63:0] _103;
    wire [63:0] _102;
    wire [63:0] _101;
    wire [63:0] _100;
    wire [63:0] _99;
    wire [63:0] _98;
    wire [511:0] _106;
    (* keep="TRUE" *)
    reg [511:0] _112;
    (* keep="TRUE" *)
    reg [511:0] _115;
    (* keep="TRUE" *)
    reg [511:0] _118;
    reg [511:0] _121;
    reg [511:0] _124;
    wire [8:0] _94 = 9'b000000000;
    wire [8:0] _93 = 9'b000000000;
    wire [8:0] _91 = 9'b000000000;
    wire [8:0] _90 = 9'b000000000;
    wire [8:0] _88 = 9'b000000000;
    wire [8:0] _87 = 9'b000000000;
    wire [8:0] _86;
    (* keep="TRUE" *)
    reg [8:0] _89;
    (* keep="TRUE" *)
    reg [8:0] _92;
    (* keep="TRUE" *)
    reg [8:0] _95;
    wire _84 = 1'b0;
    wire _83 = 1'b0;
    wire _81 = 1'b0;
    wire _80 = 1'b0;
    wire _78 = 1'b0;
    wire _77 = 1'b0;
    wire _76;
    (* keep="TRUE" *)
    reg _79;
    (* keep="TRUE" *)
    reg _82;
    (* keep="TRUE" *)
    reg _85;
    wire [8:0] _74 = 9'b000000000;
    wire [8:0] _73 = 9'b000000000;
    wire [8:0] _71 = 9'b000000000;
    wire [8:0] _70 = 9'b000000000;
    wire [8:0] _69;
    (* keep="TRUE" *)
    reg [8:0] _72;
    (* keep="TRUE" *)
    reg [8:0] _75;
    wire _67 = 1'b0;
    wire _66 = 1'b0;
    wire _64 = 1'b0;
    wire _63 = 1'b0;
    wire _62;
    (* keep="TRUE" *)
    reg _65;
    (* keep="TRUE" *)
    reg _68;
    wire [63:0] _61;
    wire [127:0] _59;
    wire [63:0] _60;
    wire [191:0] _57;
    wire [63:0] _58;
    wire [255:0] _55;
    wire [63:0] _56;
    wire [319:0] _53;
    wire [63:0] _54;
    wire [383:0] _51;
    wire [63:0] _52;
    wire [447:0] _49;
    wire [63:0] _50;
    wire [511:0] _46 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _45 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [511:0] _42 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _41 = 512'b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;
    wire [511:0] _8;
    reg [511:0] _44;
    reg [511:0] _47;
    wire [63:0] _48;
    wire _40;
    wire _39;
    wire _38;
    wire [512:0] _97;
    wire _125;
    wire _9;
    wire _30;
    wire _126;
    wire _10;
    wire _12;
    wire _14;
    wire [11:0] _26;
    wire _27;
    wire _16;
    wire [5:0] _32;
    wire _127;
    wire _17;
    wire _19;
    wire _21;
    wire _23;
    wire [12:0] _29;
    wire _128;

    /* logic */
    assign _33 = _32[0:0];
    assign _34 = _26[1:1];
    assign _110 = _76 == _109;
    assign _111 = ~ _110;
    assign _105 = _97[64:1];
    assign _104 = _97[128:65];
    assign _103 = _97[192:129];
    assign _102 = _97[256:193];
    assign _101 = _97[320:257];
    assign _100 = _97[384:321];
    assign _99 = _97[448:385];
    assign _98 = _97[512:449];
    assign _106 = { _98, _99, _100, _101, _102, _103, _104, _105 };
    always @(posedge _23) begin
        if (_111)
            _112 <= _106;
    end
    always @(posedge _23) begin
        if (_111)
            _115 <= _112;
    end
    always @(posedge _23) begin
        if (_111)
            _118 <= _115;
    end
    always @(posedge _23) begin
        if (_111)
            _121 <= _118;
    end
    always @(posedge _23) begin
        if (_111)
            _124 <= _121;
    end
    assign _86 = _29[10:2];
    always @(posedge _23) begin
        _89 <= _86;
    end
    always @(posedge _23) begin
        _92 <= _89;
    end
    always @(posedge _23) begin
        _95 <= _92;
    end
    assign _76 = _29[11:11];
    always @(posedge _23) begin
        _79 <= _76;
    end
    always @(posedge _23) begin
        _82 <= _79;
    end
    always @(posedge _23) begin
        _85 <= _82;
    end
    assign _69 = _26[10:2];
    always @(posedge _23) begin
        _72 <= _69;
    end
    always @(posedge _23) begin
        _75 <= _72;
    end
    assign _62 = _26[11:11];
    always @(posedge _23) begin
        _65 <= _62;
    end
    always @(posedge _23) begin
        _68 <= _65;
    end
    assign _61 = _59[127:64];
    assign _59 = _57[191:64];
    assign _60 = _59[63:0];
    assign _57 = _55[255:64];
    assign _58 = _57[63:0];
    assign _55 = _53[319:64];
    assign _56 = _55[63:0];
    assign _53 = _51[383:64];
    assign _54 = _53[63:0];
    assign _51 = _49[447:64];
    assign _52 = _51[63:0];
    assign _49 = _47[511:64];
    assign _50 = _49[63:0];
    assign _8 = data_in_tdata;
    always @(posedge _23) begin
        _44 <= _8;
    end
    always @(posedge _23) begin
        _47 <= _44;
    end
    assign _48 = _47[63:0];
    assign _40 = _32[5:5];
    assign _39 = _32[4:4];
    assign _38 = _32[3:3];
    multi_parallel_cores
        multi_parallel_cores
        ( .clock(_23), .clear(_21), .start(_38), .first_4step_pass(_14), .first_iter(_39), .flip(_40), .wr_d_0_0(_48), .wr_d_0_1(_50), .wr_d_0_2(_52), .wr_d_0_3(_54), .wr_d_0_4(_56), .wr_d_0_5(_58), .wr_d_0_6(_60), .wr_d_0_7(_61), .wr_en(_68), .wr_addr0(_75), .rd_en(_85), .rd_addr0(_95), .rd_d_0_7(_97[512:449]), .rd_d_0_6(_97[448:385]), .rd_d_0_5(_97[384:321]), .rd_d_0_4(_97[320:257]), .rd_d_0_3(_97[256:193]), .rd_d_0_2(_97[192:129]), .rd_d_0_1(_97[128:65]), .rd_d_0_0(_97[64:1]), .done_(_97[0:0]) );
    assign _125 = _97[0:0];
    assign _9 = _125;
    assign _30 = _29[0:0];
    assign _126 = _32[1:1];
    assign _10 = _126;
    assign _12 = data_in_tvalid;
    assign _14 = first_4step_pass;
    load_sm
        load_sm
        ( .clock(_23), .clear(_21), .first_4step_pass(_14), .tvalid(_12), .start(_10), .wr_en(_26[11:11]), .wr_addr(_26[10:2]), .tready(_26[1:1]), .done_(_26[0:0]) );
    assign _27 = _26[0:0];
    assign _16 = start;
    controller
        controller
        ( .clock(_23), .clear(_21), .start(_16), .input_done(_27), .output_done(_30), .cores_done(_9), .flip(_32[5:5]), .first_iter(_32[4:4]), .start_cores(_32[3:3]), .start_output(_32[2:2]), .start_input(_32[1:1]), .done_(_32[0:0]) );
    assign _127 = _32[2:2];
    assign _17 = _127;
    assign _19 = data_out_dest_tready;
    assign _21 = clear;
    assign _23 = clock;
    store_sm
        store_sm
        ( .clock(_23), .clear(_21), .tready(_19), .start(_17), .block(_29[12:12]), .rd_en(_29[11:11]), .rd_addr(_29[10:2]), .tvalid(_29[1:1]), .done_(_29[0:0]) );
    assign _128 = _29[1:1];

    /* aliases */

    /* output assignments */
    assign data_out_tvalid = _128;
    assign data_out_tdata = _124;
    assign data_out_tkeep = _37;
    assign data_out_tstrb = _36;
    assign data_out_tlast = gnd;
    assign data_in_dest_tready = _34;
    assign done_ = _33;

endmodule
module transposer_memories (
    write_enable,
    write_data,
    write_address,
    clock,
    read_address,
    read_data0,
    read_data1,
    read_data2,
    read_data3,
    read_data4,
    read_data5,
    read_data6,
    read_data7
);

    input [7:0] write_enable;
    input [511:0] write_data;
    input write_address;
    input clock;
    input read_address;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data0;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data1;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data2;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data3;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data4;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data5;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data6;
    (* RAM_STYLE="distributed" *)
    output [511:0] read_data7;

    /* signal declarations */
    wire _19;
    reg [511:0] _20[0:1];
    wire [511:0] _21;
    wire _22;
    reg [511:0] _23[0:1];
    wire [511:0] _24;
    wire _25;
    reg [511:0] _26[0:1];
    wire [511:0] _27;
    wire _28;
    reg [511:0] _29[0:1];
    wire [511:0] _30;
    wire _31;
    reg [511:0] _32[0:1];
    wire [511:0] _33;
    wire _34;
    reg [511:0] _35[0:1];
    wire [511:0] _36;
    wire _37;
    reg [511:0] _38[0:1];
    wire [511:0] _39;
    wire [7:0] _9;
    wire _40;
    wire [511:0] _11;
    wire _13;
    wire _15;
    reg [511:0] _41[0:1];
    wire _17;
    wire [511:0] _42;

    /* logic */
    assign _19 = _9[7:7];
    always @(posedge _15) begin
        if (_19)
            _20[_13] <= _11;
    end
    assign _21 = _20[_17];
    assign _22 = _9[6:6];
    always @(posedge _15) begin
        if (_22)
            _23[_13] <= _11;
    end
    assign _24 = _23[_17];
    assign _25 = _9[5:5];
    always @(posedge _15) begin
        if (_25)
            _26[_13] <= _11;
    end
    assign _27 = _26[_17];
    assign _28 = _9[4:4];
    always @(posedge _15) begin
        if (_28)
            _29[_13] <= _11;
    end
    assign _30 = _29[_17];
    assign _31 = _9[3:3];
    always @(posedge _15) begin
        if (_31)
            _32[_13] <= _11;
    end
    assign _33 = _32[_17];
    assign _34 = _9[2:2];
    always @(posedge _15) begin
        if (_34)
            _35[_13] <= _11;
    end
    assign _36 = _35[_17];
    assign _37 = _9[1:1];
    always @(posedge _15) begin
        if (_37)
            _38[_13] <= _11;
    end
    assign _39 = _38[_17];
    assign _9 = write_enable;
    assign _40 = _9[0:0];
    assign _11 = write_data;
    assign _13 = write_address;
    assign _15 = clock;
    always @(posedge _15) begin
        if (_40)
            _41[_13] <= _11;
    end
    assign _17 = read_address;
    assign _42 = _41[_17];

    /* aliases */

    /* output assignments */
    assign read_data0 = _42;
    assign read_data1 = _39;
    assign read_data2 = _36;
    assign read_data3 = _33;
    assign read_data4 = _30;
    assign read_data5 = _27;
    assign read_data6 = _24;
    assign read_data7 = _21;

endmodule
module transposer (
    in_tdata,
    in_tvalid,
    clear,
    clock,
    out_tready,
    in_tkeep,
    in_tlast,
    in_tstrb,
    out_tvalid,
    out_tdata,
    out_tkeep,
    out_tstrb,
    out_tlast,
    in_tready
);

    input [511:0] in_tdata;
    input in_tvalid;
    input clear;
    input clock;
    input out_tready;
    input [63:0] in_tkeep;
    input in_tlast;
    input [63:0] in_tstrb;
    output out_tvalid;
    output [511:0] out_tdata;
    output [63:0] out_tkeep;
    output [63:0] out_tstrb;
    output out_tlast;
    output in_tready;

    /* signal declarations */
    wire _48 = 1'b0;
    wire _47 = 1'b0;
    wire _72 = 1'b1;
    wire _73;
    wire _59 = 1'b0;
    wire _60;
    wire _61;
    wire _62;
    wire _46;
    wire _63;
    wire _44;
    wire _74;
    wire _1;
    reg _49;
    wire _2;
    wire gnd = 1'b0;
    wire _4;
    wire [63:0] _76 = 64'b1111111111111111111111111111111111111111111111111111111111111111;
    wire [63:0] _6;
    wire [63:0] _77 = 64'b1111111111111111111111111111111111111111111111111111111111111111;
    wire [63:0] _8;
    wire [63:0] _220;
    wire [127:0] _218;
    wire [63:0] _219;
    wire [191:0] _216;
    wire [63:0] _217;
    wire [255:0] _214;
    wire [63:0] _215;
    wire [319:0] _212;
    wire [63:0] _213;
    wire [383:0] _210;
    wire [63:0] _211;
    wire [447:0] _208;
    wire [63:0] _209;
    wire [511:0] _206;
    wire [63:0] _207;
    reg [63:0] _221;
    wire [63:0] _204;
    wire [127:0] _202;
    wire [63:0] _203;
    wire [191:0] _200;
    wire [63:0] _201;
    wire [255:0] _198;
    wire [63:0] _199;
    wire [319:0] _196;
    wire [63:0] _197;
    wire [383:0] _194;
    wire [63:0] _195;
    wire [447:0] _192;
    wire [63:0] _193;
    wire [511:0] _190;
    wire [63:0] _191;
    reg [63:0] _205;
    wire [63:0] _188;
    wire [127:0] _186;
    wire [63:0] _187;
    wire [191:0] _184;
    wire [63:0] _185;
    wire [255:0] _182;
    wire [63:0] _183;
    wire [319:0] _180;
    wire [63:0] _181;
    wire [383:0] _178;
    wire [63:0] _179;
    wire [447:0] _176;
    wire [63:0] _177;
    wire [511:0] _174;
    wire [63:0] _175;
    reg [63:0] _189;
    wire [63:0] _172;
    wire [127:0] _170;
    wire [63:0] _171;
    wire [191:0] _168;
    wire [63:0] _169;
    wire [255:0] _166;
    wire [63:0] _167;
    wire [319:0] _164;
    wire [63:0] _165;
    wire [383:0] _162;
    wire [63:0] _163;
    wire [447:0] _160;
    wire [63:0] _161;
    wire [511:0] _158;
    wire [63:0] _159;
    reg [63:0] _173;
    wire [63:0] _156;
    wire [127:0] _154;
    wire [63:0] _155;
    wire [191:0] _152;
    wire [63:0] _153;
    wire [255:0] _150;
    wire [63:0] _151;
    wire [319:0] _148;
    wire [63:0] _149;
    wire [383:0] _146;
    wire [63:0] _147;
    wire [447:0] _144;
    wire [63:0] _145;
    wire [511:0] _142;
    wire [63:0] _143;
    reg [63:0] _157;
    wire [63:0] _140;
    wire [127:0] _138;
    wire [63:0] _139;
    wire [191:0] _136;
    wire [63:0] _137;
    wire [255:0] _134;
    wire [63:0] _135;
    wire [319:0] _132;
    wire [63:0] _133;
    wire [383:0] _130;
    wire [63:0] _131;
    wire [447:0] _128;
    wire [63:0] _129;
    wire [511:0] _126;
    wire [63:0] _127;
    reg [63:0] _141;
    wire [63:0] _124;
    wire [127:0] _122;
    wire [63:0] _123;
    wire [191:0] _120;
    wire [63:0] _121;
    wire [255:0] _118;
    wire [63:0] _119;
    wire [319:0] _116;
    wire [63:0] _117;
    wire [383:0] _114;
    wire [63:0] _115;
    wire [447:0] _112;
    wire [63:0] _113;
    wire [511:0] _110;
    wire [63:0] _111;
    reg [63:0] _125;
    wire [63:0] _108;
    wire [127:0] _106;
    wire [63:0] _107;
    wire [191:0] _104;
    wire [63:0] _105;
    wire [255:0] _102;
    wire [63:0] _103;
    wire [319:0] _100;
    wire [63:0] _101;
    wire [383:0] _98;
    wire [63:0] _99;
    wire [447:0] _96;
    wire [63:0] _97;
    wire [511:0] _11;
    wire [511:0] _12;
    wire _78;
    wire _79;
    wire [1:0] _80;
    wire [3:0] _81;
    wire [7:0] _82;
    wire [7:0] _83;
    wire [7:0] _13;
    wire _84;
    wire _14;
    wire _88;
    wire _15;
    wire [4095:0] _93;
    wire [511:0] _94;
    wire [63:0] _95;
    reg [63:0] _109;
    wire [511:0] _222;
    wire [511:0] _16;
    wire _312 = 1'b0;
    wire _311 = 1'b0;
    wire _319 = 1'b1;
    wire _320;
    wire _314 = 1'b0;
    wire _315;
    wire _316;
    wire _317;
    wire _310;
    wire _318;
    wire _260 = 1'b0;
    wire _259 = 1'b0;
    wire [1:0] _303 = 2'b10;
    wire [1:0] _304;
    wire _305;
    wire [1:0] _300 = 2'b01;
    wire [1:0] _65 = 2'b00;
    wire [1:0] _64 = 2'b00;
    wire [1:0] _253 = 2'b01;
    wire [1:0] _254;
    wire [1:0] _255;
    wire [1:0] _256;
    wire [1:0] _257;
    wire _40 = 1'b0;
    wire _39 = 1'b0;
    wire _250;
    wire [7:0] _56 = 8'b00000000;
    wire [7:0] _55 = 8'b00000000;
    wire [7:0] _234 = 8'b00000001;
    wire [7:0] _233 = 8'b00000000;
    wire [1:0] _68 = 2'b01;
    wire [1:0] _69;
    wire _70;
    wire [1:0] rd_pos;
    wire _67;
    wire _71;
    wire [7:0] _235;
    wire [7:0] _228 = 8'b00000000;
    wire _226 = 1'b0;
    wire [6:0] _225;
    wire [7:0] _227;
    wire [7:0] _229;
    wire [7:0] _230;
    wire [7:0] _231;
    wire _224;
    wire [7:0] _232;
    wire _223;
    wire [7:0] _236;
    wire [7:0] _19;
    reg [7:0] _57;
    wire _58;
    wire _246;
    wire _53 = 1'b0;
    wire _51 = 1'b0;
    wire _50 = 1'b0;
    wire _240 = 1'b0;
    wire _238 = 1'b1;
    wire _239;
    wire _241;
    wire _242;
    wire _237;
    wire _243;
    wire _20;
    reg _52;
    wire _54;
    wire _247;
    wire _22;
    wire _248;
    wire _45 = 1'b1;
    wire _245;
    wire _249;
    wire _43 = 1'b0;
    wire _244;
    wire _251;
    wire _23;
    reg _42;
    wire _252;
    wire [1:0] _258;
    wire [1:0] _24;
    reg [1:0] _66;
    wire [1:0] wr_pos;
    wire [1:0] _301;
    wire [1:0] _86 = 2'b00;
    wire [1:0] _85 = 2'b00;
    wire [1:0] _271 = 2'b01;
    wire [1:0] _272;
    wire [1:0] _273;
    wire [1:0] _274;
    wire [1:0] _275;
    wire _263;
    wire [1:0] _276;
    wire [1:0] _26;
    reg [1:0] _87;
    wire _302;
    wire _306;
    wire _307;
    wire _269 = 1'b0;
    wire _267 = 1'b0;
    wire _266 = 1'b0;
    wire _287 = 1'b0;
    wire _282 = 1'b0;
    wire _280 = 1'b1;
    wire _281;
    wire _283;
    wire _284;
    wire _285;
    wire _279;
    wire _286;
    wire _278;
    wire _288;
    wire _27;
    reg _268;
    wire _270;
    wire _296;
    wire [2:0] _264 = 3'b111;
    wire vdd = 1'b1;
    wire [2:0] _90 = 3'b000;
    wire _29;
    wire [2:0] _89 = 3'b000;
    wire _31;
    wire [2:0] _290 = 3'b001;
    wire [2:0] _291;
    wire [2:0] _292;
    wire _289;
    wire [2:0] _293;
    wire [2:0] _32;
    reg [2:0] element_offset;
    wire _265;
    wire _297;
    wire _34;
    wire _298;
    wire _262 = 1'b1;
    wire _295;
    wire _299;
    wire _277 = 1'b0;
    wire _294;
    wire _308;
    wire _35;
    reg _261;
    wire _309;
    wire _321;
    wire _36;
    reg _313;
    wire _37;

    /* logic */
    assign _73 = _71 ? _72 : _49;
    assign _60 = _58 ? _59 : _49;
    assign _61 = _54 ? _60 : _49;
    assign _62 = _22 ? _61 : _49;
    assign _46 = _42 == _45;
    assign _63 = _46 ? _62 : _49;
    assign _44 = _42 == _43;
    assign _74 = _44 ? _73 : _63;
    assign _1 = _74;
    always @(posedge _31) begin
        if (_29)
            _49 <= _48;
        else
            _49 <= _1;
    end
    assign _2 = _49;
    assign _4 = gnd;
    assign _6 = _76;
    assign _8 = _77;
    assign _220 = _218[127:64];
    assign _218 = _216[191:64];
    assign _219 = _218[63:0];
    assign _216 = _214[255:64];
    assign _217 = _216[63:0];
    assign _214 = _212[319:64];
    assign _215 = _214[63:0];
    assign _212 = _210[383:64];
    assign _213 = _212[63:0];
    assign _210 = _208[447:64];
    assign _211 = _210[63:0];
    assign _208 = _206[511:64];
    assign _209 = _208[63:0];
    assign _206 = _93[511:0];
    assign _207 = _206[63:0];
    always @* begin
        case (element_offset)
        0: _221 <= _207;
        1: _221 <= _209;
        2: _221 <= _211;
        3: _221 <= _213;
        4: _221 <= _215;
        5: _221 <= _217;
        6: _221 <= _219;
        default: _221 <= _220;
        endcase
    end
    assign _204 = _202[127:64];
    assign _202 = _200[191:64];
    assign _203 = _202[63:0];
    assign _200 = _198[255:64];
    assign _201 = _200[63:0];
    assign _198 = _196[319:64];
    assign _199 = _198[63:0];
    assign _196 = _194[383:64];
    assign _197 = _196[63:0];
    assign _194 = _192[447:64];
    assign _195 = _194[63:0];
    assign _192 = _190[511:64];
    assign _193 = _192[63:0];
    assign _190 = _93[1023:512];
    assign _191 = _190[63:0];
    always @* begin
        case (element_offset)
        0: _205 <= _191;
        1: _205 <= _193;
        2: _205 <= _195;
        3: _205 <= _197;
        4: _205 <= _199;
        5: _205 <= _201;
        6: _205 <= _203;
        default: _205 <= _204;
        endcase
    end
    assign _188 = _186[127:64];
    assign _186 = _184[191:64];
    assign _187 = _186[63:0];
    assign _184 = _182[255:64];
    assign _185 = _184[63:0];
    assign _182 = _180[319:64];
    assign _183 = _182[63:0];
    assign _180 = _178[383:64];
    assign _181 = _180[63:0];
    assign _178 = _176[447:64];
    assign _179 = _178[63:0];
    assign _176 = _174[511:64];
    assign _177 = _176[63:0];
    assign _174 = _93[1535:1024];
    assign _175 = _174[63:0];
    always @* begin
        case (element_offset)
        0: _189 <= _175;
        1: _189 <= _177;
        2: _189 <= _179;
        3: _189 <= _181;
        4: _189 <= _183;
        5: _189 <= _185;
        6: _189 <= _187;
        default: _189 <= _188;
        endcase
    end
    assign _172 = _170[127:64];
    assign _170 = _168[191:64];
    assign _171 = _170[63:0];
    assign _168 = _166[255:64];
    assign _169 = _168[63:0];
    assign _166 = _164[319:64];
    assign _167 = _166[63:0];
    assign _164 = _162[383:64];
    assign _165 = _164[63:0];
    assign _162 = _160[447:64];
    assign _163 = _162[63:0];
    assign _160 = _158[511:64];
    assign _161 = _160[63:0];
    assign _158 = _93[2047:1536];
    assign _159 = _158[63:0];
    always @* begin
        case (element_offset)
        0: _173 <= _159;
        1: _173 <= _161;
        2: _173 <= _163;
        3: _173 <= _165;
        4: _173 <= _167;
        5: _173 <= _169;
        6: _173 <= _171;
        default: _173 <= _172;
        endcase
    end
    assign _156 = _154[127:64];
    assign _154 = _152[191:64];
    assign _155 = _154[63:0];
    assign _152 = _150[255:64];
    assign _153 = _152[63:0];
    assign _150 = _148[319:64];
    assign _151 = _150[63:0];
    assign _148 = _146[383:64];
    assign _149 = _148[63:0];
    assign _146 = _144[447:64];
    assign _147 = _146[63:0];
    assign _144 = _142[511:64];
    assign _145 = _144[63:0];
    assign _142 = _93[2559:2048];
    assign _143 = _142[63:0];
    always @* begin
        case (element_offset)
        0: _157 <= _143;
        1: _157 <= _145;
        2: _157 <= _147;
        3: _157 <= _149;
        4: _157 <= _151;
        5: _157 <= _153;
        6: _157 <= _155;
        default: _157 <= _156;
        endcase
    end
    assign _140 = _138[127:64];
    assign _138 = _136[191:64];
    assign _139 = _138[63:0];
    assign _136 = _134[255:64];
    assign _137 = _136[63:0];
    assign _134 = _132[319:64];
    assign _135 = _134[63:0];
    assign _132 = _130[383:64];
    assign _133 = _132[63:0];
    assign _130 = _128[447:64];
    assign _131 = _130[63:0];
    assign _128 = _126[511:64];
    assign _129 = _128[63:0];
    assign _126 = _93[3071:2560];
    assign _127 = _126[63:0];
    always @* begin
        case (element_offset)
        0: _141 <= _127;
        1: _141 <= _129;
        2: _141 <= _131;
        3: _141 <= _133;
        4: _141 <= _135;
        5: _141 <= _137;
        6: _141 <= _139;
        default: _141 <= _140;
        endcase
    end
    assign _124 = _122[127:64];
    assign _122 = _120[191:64];
    assign _123 = _122[63:0];
    assign _120 = _118[255:64];
    assign _121 = _120[63:0];
    assign _118 = _116[319:64];
    assign _119 = _118[63:0];
    assign _116 = _114[383:64];
    assign _117 = _116[63:0];
    assign _114 = _112[447:64];
    assign _115 = _114[63:0];
    assign _112 = _110[511:64];
    assign _113 = _112[63:0];
    assign _110 = _93[3583:3072];
    assign _111 = _110[63:0];
    always @* begin
        case (element_offset)
        0: _125 <= _111;
        1: _125 <= _113;
        2: _125 <= _115;
        3: _125 <= _117;
        4: _125 <= _119;
        5: _125 <= _121;
        6: _125 <= _123;
        default: _125 <= _124;
        endcase
    end
    assign _108 = _106[127:64];
    assign _106 = _104[191:64];
    assign _107 = _106[63:0];
    assign _104 = _102[255:64];
    assign _105 = _104[63:0];
    assign _102 = _100[319:64];
    assign _103 = _102[63:0];
    assign _100 = _98[383:64];
    assign _101 = _100[63:0];
    assign _98 = _96[447:64];
    assign _99 = _98[63:0];
    assign _96 = _94[511:64];
    assign _97 = _96[63:0];
    assign _11 = in_tdata;
    assign _12 = _11;
    assign _78 = _45 == _42;
    assign _79 = _78 & _22;
    assign _80 = { _79, _79 };
    assign _81 = { _80, _80 };
    assign _82 = { _81, _81 };
    assign _83 = _57 & _82;
    assign _13 = _83;
    assign _84 = _66[0:0];
    assign _14 = _84;
    assign _88 = _87[0:0];
    assign _15 = _88;
    transposer_memories
        transposer_memories
        ( .clock(_31), .read_address(_15), .write_address(_14), .write_enable(_13), .write_data(_12), .read_data7(_93[4095:3584]), .read_data6(_93[3583:3072]), .read_data5(_93[3071:2560]), .read_data4(_93[2559:2048]), .read_data3(_93[2047:1536]), .read_data2(_93[1535:1024]), .read_data1(_93[1023:512]), .read_data0(_93[511:0]) );
    assign _94 = _93[4095:3584];
    assign _95 = _94[63:0];
    always @* begin
        case (element_offset)
        0: _109 <= _95;
        1: _109 <= _97;
        2: _109 <= _99;
        3: _109 <= _101;
        4: _109 <= _103;
        5: _109 <= _105;
        6: _109 <= _107;
        default: _109 <= _108;
        endcase
    end
    assign _222 = { _109, _125, _141, _157, _173, _189, _205, _221 };
    assign _16 = _222;
    assign _320 = _306 ? _319 : _313;
    assign _315 = _270 ? _314 : _313;
    assign _316 = _265 ? _315 : _313;
    assign _317 = _34 ? _316 : _313;
    assign _310 = _261 == _262;
    assign _318 = _310 ? _317 : _313;
    assign _304 = wr_pos - _303;
    assign _305 = _87 == _304;
    assign _254 = _66 + _253;
    assign _255 = _58 ? _254 : _66;
    assign _256 = _54 ? _255 : _66;
    assign _257 = _22 ? _256 : _66;
    assign _250 = _71 ? _45 : _42;
    assign _69 = _66 - _68;
    assign _70 = rd_pos == _69;
    assign rd_pos = _87;
    assign _67 = rd_pos == _66;
    assign _71 = _67 | _70;
    assign _235 = _71 ? _234 : _233;
    assign _225 = _57[6:0];
    assign _227 = { _225, _226 };
    assign _229 = _58 ? _228 : _227;
    assign _230 = _54 ? _229 : _57;
    assign _231 = _22 ? _230 : _57;
    assign _224 = _42 == _45;
    assign _232 = _224 ? _231 : _57;
    assign _223 = _42 == _43;
    assign _236 = _223 ? _235 : _232;
    assign _19 = _236;
    always @(posedge _31) begin
        if (_29)
            _57 <= _56;
        else
            _57 <= _19;
    end
    assign _58 = _57[7:7];
    assign _246 = _58 ? _43 : _42;
    assign _239 = _52 + _238;
    assign _241 = _54 ? _240 : _239;
    assign _242 = _22 ? _241 : _52;
    assign _237 = _42 == _45;
    assign _243 = _237 ? _242 : _52;
    assign _20 = _243;
    always @(posedge _31) begin
        if (_29)
            _52 <= _51;
        else
            _52 <= _20;
    end
    assign _54 = _52 == _53;
    assign _247 = _54 ? _246 : _42;
    assign _22 = in_tvalid;
    assign _248 = _22 ? _247 : _42;
    assign _245 = _42 == _45;
    assign _249 = _245 ? _248 : _42;
    assign _244 = _42 == _43;
    assign _251 = _244 ? _250 : _249;
    assign _23 = _251;
    always @(posedge _31) begin
        if (_29)
            _42 <= _40;
        else
            _42 <= _23;
    end
    assign _252 = _42 == _45;
    assign _258 = _252 ? _257 : _66;
    assign _24 = _258;
    always @(posedge _31) begin
        if (_29)
            _66 <= _65;
        else
            _66 <= _24;
    end
    assign wr_pos = _66;
    assign _301 = wr_pos - _300;
    assign _272 = _87 + _271;
    assign _273 = _270 ? _272 : _87;
    assign _274 = _265 ? _273 : _87;
    assign _275 = _34 ? _274 : _87;
    assign _263 = _261 == _262;
    assign _276 = _263 ? _275 : _87;
    assign _26 = _276;
    always @(posedge _31) begin
        if (_29)
            _87 <= _86;
        else
            _87 <= _26;
    end
    assign _302 = _87 == _301;
    assign _306 = _302 | _305;
    assign _307 = _306 ? _262 : _261;
    assign _281 = _268 + _280;
    assign _283 = _270 ? _282 : _281;
    assign _284 = _265 ? _283 : _268;
    assign _285 = _34 ? _284 : _268;
    assign _279 = _261 == _262;
    assign _286 = _279 ? _285 : _268;
    assign _278 = _261 == _277;
    assign _288 = _278 ? _287 : _286;
    assign _27 = _288;
    always @(posedge _31) begin
        if (_29)
            _268 <= _267;
        else
            _268 <= _27;
    end
    assign _270 = _268 == _269;
    assign _296 = _270 ? _277 : _261;
    assign _29 = clear;
    assign _31 = clock;
    assign _291 = element_offset + _290;
    assign _292 = _34 ? _291 : element_offset;
    assign _289 = _261 == _262;
    assign _293 = _289 ? _292 : element_offset;
    assign _32 = _293;
    always @(posedge _31) begin
        if (_29)
            element_offset <= _90;
        else
            element_offset <= _32;
    end
    assign _265 = element_offset == _264;
    assign _297 = _265 ? _296 : _261;
    assign _34 = out_tready;
    assign _298 = _34 ? _297 : _261;
    assign _295 = _261 == _262;
    assign _299 = _295 ? _298 : _261;
    assign _294 = _261 == _277;
    assign _308 = _294 ? _307 : _299;
    assign _35 = _308;
    always @(posedge _31) begin
        if (_29)
            _261 <= _260;
        else
            _261 <= _35;
    end
    assign _309 = _261 == _277;
    assign _321 = _309 ? _320 : _318;
    assign _36 = _321;
    always @(posedge _31) begin
        if (_29)
            _313 <= _312;
        else
            _313 <= _36;
    end
    assign _37 = _313;

    /* aliases */

    /* output assignments */
    assign out_tvalid = _37;
    assign out_tdata = _16;
    assign out_tkeep = _8;
    assign out_tstrb = _6;
    assign out_tlast = _4;
    assign in_tready = _2;

endmodule
module krnl_ntt (
    compute_to_controller_tready,
    controller_to_compute_phase_1_tlast,
    controller_to_compute_phase_1_tstrb,
    controller_to_compute_phase_1_tkeep,
    controller_to_compute_phase_1_tdata,
    controller_to_compute_phase_2_tlast,
    controller_to_compute_phase_2_tstrb,
    controller_to_compute_phase_2_tkeep,
    controller_to_compute_phase_2_tdata,
    controller_to_compute_phase_2_tvalid,
    controller_to_compute_phase_1_tvalid,
    ap_rst_n,
    ap_clk,
    compute_to_controller_tvalid,
    compute_to_controller_tdata,
    compute_to_controller_tkeep,
    compute_to_controller_tstrb,
    compute_to_controller_tlast,
    controller_to_compute_phase_1_tready,
    controller_to_compute_phase_2_tready
);

    input compute_to_controller_tready;
    input controller_to_compute_phase_1_tlast;
    input [63:0] controller_to_compute_phase_1_tstrb;
    input [63:0] controller_to_compute_phase_1_tkeep;
    input [511:0] controller_to_compute_phase_1_tdata;
    input controller_to_compute_phase_2_tlast;
    input [63:0] controller_to_compute_phase_2_tstrb;
    input [63:0] controller_to_compute_phase_2_tkeep;
    input [511:0] controller_to_compute_phase_2_tdata;
    input controller_to_compute_phase_2_tvalid;
    input controller_to_compute_phase_1_tvalid;
    input ap_rst_n;
    input ap_clk;
    output compute_to_controller_tvalid;
    output [511:0] compute_to_controller_tdata;
    output [63:0] compute_to_controller_tkeep;
    output [63:0] compute_to_controller_tstrb;
    output compute_to_controller_tlast;
    output controller_to_compute_phase_1_tready;
    output controller_to_compute_phase_2_tready;

    /* signal declarations */
    wire _41;
    wire _57;
    wire [63:0] _58;
    wire [63:0] _59;
    wire [511:0] _60;
    wire _8;
    wire _10;
    wire _53;
    wire _54;
    wire [63:0] _12;
    wire [63:0] _51;
    wire [63:0] _52;
    wire [63:0] _14;
    wire [63:0] _49;
    wire [63:0] _50;
    wire [511:0] _16;
    wire [511:0] _47;
    wire [511:0] _48;
    wire _61;
    wire _17;
    wire _18;
    wire _20;
    wire [63:0] _22;
    wire [63:0] _24;
    wire [511:0] _26;
    wire [642:0] _40;
    wire _45;
    wire _46;
    wire _43 = 1'b0;
    wire _42 = 1'b0;
    wire _62;
    wire _27;
    reg _4STEP;
    wire _29;
    wire _31;
    wire _64;
    wire _63;
    wire _65;
    wire _32;
    wire _34;
    wire _38;
    wire _36;
    wire [643:0] _56;
    wire _66;

    /* logic */
    assign _41 = _40[642:642];
    assign _57 = _56[641:641];
    assign _58 = _56[640:577];
    assign _59 = _56[576:513];
    assign _60 = _56[512:1];
    assign _8 = compute_to_controller_tready;
    assign _10 = controller_to_compute_phase_1_tlast;
    assign _53 = _40[641:641];
    assign _54 = _31 ? _10 : _53;
    assign _12 = controller_to_compute_phase_1_tstrb;
    assign _51 = _40[640:577];
    assign _52 = _31 ? _12 : _51;
    assign _14 = controller_to_compute_phase_1_tkeep;
    assign _49 = _40[576:513];
    assign _50 = _31 ? _14 : _49;
    assign _16 = controller_to_compute_phase_1_tdata;
    assign _47 = _40[512:1];
    assign _48 = _31 ? _16 : _47;
    assign _61 = _56[642:642];
    assign _17 = _61;
    assign _18 = _17;
    assign _20 = controller_to_compute_phase_2_tlast;
    assign _22 = controller_to_compute_phase_2_tstrb;
    assign _24 = controller_to_compute_phase_2_tkeep;
    assign _26 = controller_to_compute_phase_2_tdata;
    transposer
        transposer
        ( .clock(_36), .clear(_38), .in_tvalid(_29), .in_tdata(_26), .in_tkeep(_24), .in_tstrb(_22), .in_tlast(_20), .out_tready(_18), .in_tready(_40[642:642]), .out_tlast(_40[641:641]), .out_tstrb(_40[640:577]), .out_tkeep(_40[576:513]), .out_tdata(_40[512:1]), .out_tvalid(_40[0:0]) );
    assign _45 = _40[0:0];
    assign _46 = _31 ? _31 : _45;
    assign _62 = ~ _4STEP;
    assign _27 = _62;
    always @(posedge _36) begin
        if (_38)
            _4STEP <= _43;
        else
            if (_32)
                _4STEP <= _27;
    end
    assign _29 = controller_to_compute_phase_2_tvalid;
    assign _31 = controller_to_compute_phase_1_tvalid;
    assign _64 = _31 | _29;
    assign _63 = _56[643:643];
    assign _65 = _63 & _64;
    assign _32 = _65;
    assign _34 = ap_rst_n;
    assign _38 = ~ _34;
    assign _36 = ap_clk;
    kernel
        kernel
        ( .clock(_36), .clear(_38), .start(_32), .first_4step_pass(_4STEP), .data_in_tvalid(_46), .data_in_tdata(_48), .data_in_tkeep(_50), .data_in_tstrb(_52), .data_in_tlast(_54), .data_out_dest_tready(_8), .done_(_56[643:643]), .data_in_dest_tready(_56[642:642]), .data_out_tlast(_56[641:641]), .data_out_tstrb(_56[640:577]), .data_out_tkeep(_56[576:513]), .data_out_tdata(_56[512:1]), .data_out_tvalid(_56[0:0]) );
    assign _66 = _56[0:0];

    /* aliases */

    /* output assignments */
    assign compute_to_controller_tvalid = _66;
    assign compute_to_controller_tdata = _60;
    assign compute_to_controller_tkeep = _59;
    assign compute_to_controller_tstrb = _58;
    assign compute_to_controller_tlast = _57;
    assign controller_to_compute_phase_1_tready = _17;
    assign controller_to_compute_phase_2_tready = _41;

endmodule
