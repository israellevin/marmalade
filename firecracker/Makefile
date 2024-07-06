# Define variables
BIN_DIR = /usr/local/bin
ASSETS_DIR = /usr/local/share/marmalade
BUILD_DIR = build

all: install

install:
	mkdir -p $(ASSETS_DIR)
	sudo cp firecracker_config.json $(ASSETS_DIR)
	sudo cp $(BUILD_DIR)/vmlinux $(ASSETS_DIR)
	sudo cp $(BUILD_DIR)/generator_base.ext4 $(ASSETS_DIR)
	sudo cp $(BUILD_DIR)/firecracker $(BIN_DIR)
	sudo cp $(BUILD_DIR)/marmalade-nslaunch $(BIN_DIR)
	sudo chmod 4755 $(BIN_DIR)/marmalade-nslaunch

uninstall:
	sudo rm -f $(ASSETS_DIR)/firecracker_config.json
	sudo rm -f $(ASSETS_DIR)/vmlinux
	sudo rm -f $(ASSETS_DIR)/generator_base.ext4
	sudo rm -f $(BIN_DIR)/firecracker
	sudo rm -f $(BIN_DIR)/marmalade-nslaunch
