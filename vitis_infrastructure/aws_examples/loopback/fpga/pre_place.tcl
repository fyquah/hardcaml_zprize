create_pblock pblock_SLR2
resize_pblock pblock_SLR2 -add SLR2
add_cells_to_pblock pblock_SLR2 [get_cells -hierarchical *krnl_loopback*]
