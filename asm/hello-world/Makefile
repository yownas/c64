name := hello-world

all: build

clean:
	rm -f ${name}.prg

poweroff:
	curl -vX PUT "http://c64/v1/machine:poweroff"

reset:
	curl -vX PUT "http://c64/v1/machine:reset"

run: build
	curl -vF "file=@${name}.prg" "http://c64/v1/runners:run_prg"

${name}.prg:
	acme -v9 ${name}.asm

build: ${name}.prg
