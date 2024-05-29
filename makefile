FISH_CORE_HOME := $(shell pwd)


Linux_payload := ${FISH_CORE_HOME}/sim/ready_to_run/fw_payload.elf
sim_dir := ${FISH_CORE_HOME}/sim


gen_fish_soc_verilog:
	sbt "runMain  leesum.Core.gen_FishSoc"

build_sim:gen_fish_soc_verilog
	cd ${sim_dir} && xmake


linux:build_sim
	cd ${sim_dir} && xmake run Vtop --file ${Linux_payload} --clk 500000000


.PHONY: gen_verilog
