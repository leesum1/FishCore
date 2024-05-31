FISH_CORE_HOME := $(shell pwd)


Linux_payload := ${FISH_CORE_HOME}/sim/ready_to_run/fw_payload.elf
sim_dir := ${FISH_CORE_HOME}/sim

clean_dir := ${FISH_CORE_HOME}/project \
			 ${FISH_CORE_HOME}/.venv \
			 ${FISH_CORE_HOME}/target \
			 ${FISH_CORE_HOME}/sim/.xmake \
			 ${FISH_CORE_HOME}/sim/.cache \
			 ${FISH_CORE_HOME}/sim/build \

define check_and_create_folder
    @if [ ! -d "$(1)" ]; then \
        echo "Creating folder $(1)"; \
        mkdir -p $(1); \
    fi
endef


gen_fish_soc_verilog:
	$(call check_and_create_folder,${FISH_CORE_HOME}/sim/vsrc)
	cd ${FISH_CORE_HOME} && sbt "runMain  leesum.Core.gen_FishSoc"

build_sim:gen_fish_soc_verilog
	cd ${sim_dir} && xmake


linux:build_sim
	cd ${sim_dir} && xmake run Vtop --file ${Linux_payload} --clk 500000000

clean_all:
	rm -rf ${clean_dir}


.PHONY: gen_verilog build_sim linux
