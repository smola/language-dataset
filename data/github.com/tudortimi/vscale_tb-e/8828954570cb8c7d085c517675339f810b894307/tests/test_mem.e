<'
import e/vscale_tb_top;


extend vgm_ahb::transfer {
  keep delay == 1;
};


extend MAIN vgm_risc_v::instruction_stream {
  !sw : SW'kind vgm_risc_v::instruction;
  !lw : LW'kind vgm_risc_v::instruction;

  body() @driver.clock is only {
    do sw keeping {
      .args.rs1 == x0;
      .args.imm == 0x0100;
      .args.rs2 == x1;
    };

    // TODO Bug?
    wait [5];

    do lw keeping {
      .args.rd == x3;
      .args.rs1 == x0;
      .args.imm == 0x100;
    };
  };
};


extend DMEM MAIN vgm_ahb::slave_sequence {
  !seq_item : vgm_ahb::transfer;

  body() @driver.clock is only {
    do seq_item;
    do seq_item keeping { .data == 0xdead_beef };
  };
};
'>
