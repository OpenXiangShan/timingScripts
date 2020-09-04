.PHONY: all test clean

SRCS = $(shell find -name "*.scala")
OUT_DIR = build
PACKAGE = scalaTage

all: $(SRCS)
	@mkdir -p $(OUT_DIR)
	scalac $^ -d $(OUT_DIR)

test:
	@cd $(OUT_DIR) && scala $(PACKAGE).Test

clean:
	rm -rf $(OUT_DIR)
