#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main() {
    if (setuid(0) != 0) {
        perror("setuid failed");
        return 1;
    }
    execl("/usr/local/bin/marmalade_tap.sh", "/usr/local/bin/marmalade_tap.sh", (char *)NULL);
    perror("execl failed");
    return 1;
}
