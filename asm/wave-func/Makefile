name := wave-func
c64u := c64

all: build

clean:
	rm -f ${name}.prg

poweroff:
	curl -vX PUT "http://${c64u}/v1/machine:poweroff"

reset:
	curl -vX PUT "http://${c64u}/v1/machine:reset"

run: build
	curl -vF "file=@${name}.prg" "http://${c64u}/v1/runners:run_prg"

${name}.prg: ${name}.asm
	acme -v9 ${name}.asm

build: ${name}.prg
