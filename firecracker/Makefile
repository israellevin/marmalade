# Define variables
BIN_DIR = /usr/local/bin
ASSETS_DIR = /usr/local/etc/marmalade
BUILD_DIR = artifacts
GROUP_NAME = marmalade

build:
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR) && ../build.sh $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

marmalade-group:
	@if ! getent group $(GROUP_NAME) > /dev/null; then \
		echo "Creating group $(GROUP_NAME)"; \
		sudo groupadd $(GROUP_NAME); \
	else \
		echo "Group $(GROUP_NAME) already exists"; \
	fi

	@for user in $(SUDO_USER) $(filter-out $@, $(MAKECMDGOALS)); do \
		if ! id -nG "$$user" | grep -qw "$(GROUP_NAME)"; then \
			echo "Adding user $$user to group $(GROUP_NAME)"; \
			sudo usermod -aG $(GROUP_NAME) $$user; \
		else \
			echo "User $$user is already a member of group $(GROUP_NAME)"; \
		fi \
	done

	sudo bash -c \
		'echo "%$(GROUP_NAME) ALL=(ALL) NOPASSWD: /usr/local/bin/marmalade-nslaunch" > /etc/sudoers.d/$(GROUP_NAME)'

install:
	[ -d $(BUILD_DIR) ] || make build
	make marmalade-group
	sudo mkdir -p $(BIN_DIR)
	sudo cp $(BUILD_DIR)/firecracker $(BIN_DIR)
	sudo cp marmalade-nslaunch $(BIN_DIR)/marmalade-nslaunch
	sudo chmod 550 \
		$(BIN_DIR)/firecracker \
		$(BIN_DIR)/marmalade-nslaunch
	sudo mkdir -p $(ASSETS_DIR)
	sudo cp $(BUILD_DIR)/vmlinux $(ASSETS_DIR)
	sudo cp $(BUILD_DIR)/generator_base.ext4 $(ASSETS_DIR)
	sudo cp firecracker_config.json $(ASSETS_DIR)
	sudo chmod 440 \
		$(ASSETS_DIR)/vmlinux \
		$(ASSETS_DIR)/generator_base.ext4 \
		$(ASSETS_DIR)/firecracker_config.json
	sudo chown root:marmalade \
		$(BIN_DIR)/firecracker \
		$(BIN_DIR)/marmalade-nslaunch \
		$(ASSETS_DIR)/firecracker_config.json \
		$(ASSETS_DIR)/generator_base.ext4 \
		$(ASSETS_DIR)/vmlinux

uninstall:
	sudo rm -f $(BIN_DIR)/firecracker
	sudo rm -f $(BIN_DIR)/marmalade-nslaunch
	sudo rm -f $(ASSETS_DIR)/firecracker_config.json
	sudo rm -f $(ASSETS_DIR)/generator_base.ext4
	sudo rm -f $(ASSETS_DIR)/vmlinux
