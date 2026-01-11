name := hello-world

all: run

clean:
	rm -f ${name}.prg

poweroff:
	curl -vX PUT "http://c64/v1/machine:poweroff"

reset:
	curl -vX PUT "http://c64/v1/machine:reset"

run: build
	curl -vF "file=@${name}.prg" "http://c64/v1/runners:run_prg"

build: hello-world.asm
	acme -v9 ${name}.asm
