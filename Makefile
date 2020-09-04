.PHONY: all test clean

SRCS = $(shell find -name "*.scala")
OUT_DIR = build
PACKAGE = scalaTage

all: $(SRCS)
	@mkdir -p $(OUT_DIR)
	scalac $^ -d $(OUT_DIR)

# test: $(shell find $(OUT_DIR)/$(PACKAGE) -name "*Test*")
# 	echo $<
# 	scala -cp . $(OUT_DIR)/$(PACKAGE).$(basename $(<F))

clean:
	rm -rf $(OUT_DIR)
