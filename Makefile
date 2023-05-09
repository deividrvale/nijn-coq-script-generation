CLONE_DIR := ./experiments/nijn_lib
NIJN_REPO := https://github.com/nmvdw/Nijn

all : build_onijn build_nijn

build_onijn :
	dune build

build_nijn : $(CLONE_DIR)

$(CLONE_DIR) :
	@echo "Cloning nijn Coq library..."
	@mkdir -p $(CLONE_DIR)
	git clone $(NIJN_REPO) $(CLONE_DIR)
	@echo "Building Nijn Coq Library..."
	cd $(CLONE_DIR)/src && coq_makefile -f _CoqProject -o Makefile
	cd $(CLONE_DIR)/src && $(MAKE) && $(MAKE) install

.PHONY : clean

clean :
	rm -rf _build
	rm -rf ./experiments/nijn_lib
	rm -rf ./experiments/ho_poly_certificates
	rm -rf .nia.cache
