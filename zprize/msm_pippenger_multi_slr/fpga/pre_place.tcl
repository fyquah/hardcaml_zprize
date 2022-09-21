add_cells_to_pblock pblock_dynamic_SLR1 [get_cells -hierarchical *krnl_mm2s*]
add_cells_to_pblock pblock_dynamic_SLR1 [get_cells -hierarchical *krnl_s2mm*]

add_cells_to_pblock pblock_dynamic_SLR0 [get_cells -hierarchical *krnl_msm_pippenger*SLR0]
add_cells_to_pblock pblock_dynamic_SLR1 [get_cells -hierarchical *krnl_msm_pippenger*SLR1]
add_cells_to_pblock pblock_dynamic_SLR2 [get_cells -hierarchical *krnl_msm_pippenger*SLR2]
