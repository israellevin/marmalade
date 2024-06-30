# Define variables
CC = gcc
CFLAGS = -o
TARGET = marmalade_tap
SCRIPT = marmalade_tap.sh
BIN_DIR = /usr/local/bin

all: build install

build: $(TARGET).c
	$(CC) $(TARGET).c $(CFLAGS) $(TARGET)

clean:
	rm -f $(TARGET)

install: build
	sudo cp $(TARGET) $(BIN_DIR)
	sudo cp $(SCRIPT) $(BIN_DIR)
	sudo chown root:root $(BIN_DIR)/$(TARGET)
	sudo chmod 4755 $(BIN_DIR)/$(TARGET)
	sudo chown root:root $(BIN_DIR)/$(SCRIPT)
	sudo chmod 755 $(BIN_DIR)/$(SCRIPT)

uninstall:
	sudo rm -f $(BIN_DIR)/$(TARGET)
	sudo rm -f $(BIN_DIR)/$(SCRIPT)
