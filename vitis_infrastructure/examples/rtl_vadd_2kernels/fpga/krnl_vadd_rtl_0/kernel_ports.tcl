set core [ipx::current_core]

ipx::associate_bus_interfaces -busif m_axi_gmem -clock ap_clk $core
ipx::associate_bus_interfaces -busif s_axi_control -clock ap_clk $core

set mem_map    [::ipx::add_memory_map -quiet "s_axi_control" $core]
set addr_block [::ipx::add_address_block -quiet "reg0" $mem_map]

set reg      [::ipx::add_register "CTRL" $addr_block]
  set_property description    "Control signals"    $reg
  set_property address_offset 0x000 $reg
  set_property size           32    $reg
set field [ipx::add_field AP_START $reg]
  set_property ACCESS {read-write} $field
  set_property BIT_OFFSET {0} $field
  set_property BIT_WIDTH {1} $field
  set_property DESCRIPTION {Control signal Register for 'ap_start'.} $field
  set_property MODIFIED_WRITE_VALUE {modify} $field
set field [ipx::add_field AP_DONE $reg]
  set_property ACCESS {read-only} $field
  set_property BIT_OFFSET {1} $field
  set_property BIT_WIDTH {1} $field
  set_property DESCRIPTION {Control signal Register for 'ap_done'.} $field
  set_property READ_ACTION {modify} $field
set field [ipx::add_field AP_IDLE $reg]
  set_property ACCESS {read-only} $field
  set_property BIT_OFFSET {2} $field
  set_property BIT_WIDTH {1} $field
  set_property DESCRIPTION {Control signal Register for 'ap_idle'.} $field
  set_property READ_ACTION {modify} $field
set field [ipx::add_field AP_READY $reg]
  set_property ACCESS {read-only} $field
  set_property BIT_OFFSET {3} $field
  set_property BIT_WIDTH {1} $field
  set_property DESCRIPTION {Control signal Register for 'ap_ready'.} $field
  set_property READ_ACTION {modify} $field
set field [ipx::add_field RESERVED_1 $reg]
  set_property ACCESS {read-only} $field
  set_property BIT_OFFSET {4} $field
  set_property BIT_WIDTH {3} $field
  set_property DESCRIPTION {Reserved.  0s on read.} $field
  set_property READ_ACTION {modify} $field
set field [ipx::add_field AUTO_RESTART $reg]
  set_property ACCESS {read-write} $field
  set_property BIT_OFFSET {7} $field
  set_property BIT_WIDTH {1} $field
  set_property DESCRIPTION {Control signal Register for 'auto_restart'.} $field
  set_property MODIFIED_WRITE_VALUE {modify} $field
set field [ipx::add_field RESERVED_2 $reg]
  set_property ACCESS {read-only} $field
  set_property BIT_OFFSET {8} $field
  set_property BIT_WIDTH {24} $field
  set_property DESCRIPTION {Reserved.  0s on read.} $field
  set_property READ_ACTION {modify} $field

set reg      [::ipx::add_register "GIER" $addr_block]
  set_property description    "Global Interrupt Enable Register"    $reg
  set_property address_offset 0x004 $reg
  set_property size           32    $reg

set reg      [::ipx::add_register "IP_IER" $addr_block]
  set_property description    "IP Interrupt Enable Register"    $reg
  set_property address_offset 0x008 $reg
  set_property size           32    $reg

set reg      [::ipx::add_register "IP_ISR" $addr_block]
  set_property description    "IP Interrupt Status Register"    $reg
  set_property address_offset 0x00C $reg
  set_property size           32    $reg

set reg      [::ipx::add_register -quiet "a" $addr_block]
  set_property address_offset 0x010 $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg] 
  set_property value m_axi_gmem $regparam 

set reg      [::ipx::add_register -quiet "b" $addr_block]
  set_property address_offset 0x01C $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg] 
  set_property value m_axi_gmem $regparam 

set reg      [::ipx::add_register -quiet "c" $addr_block]
  set_property address_offset 0x028 $reg
  set_property size           [expr {8*8}]   $reg
  set regparam [::ipx::add_register_parameter -quiet {ASSOCIATED_BUSIF} $reg] 
  set_property value m_axi_gmem $regparam 

set reg      [::ipx::add_register -quiet "length_r" $addr_block]
  set_property address_offset 0x034 $reg
  set_property size           [expr {4*8}]   $reg


set_property slave_memory_map_ref "s_axi_control" [::ipx::get_bus_interfaces -of $core "s_axi_control"]
