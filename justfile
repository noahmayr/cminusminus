example_dir := "./examples"
example := "helloworld"
entry := "main.cmm"
source_path := example_dir / example / entry
out_path := if example == "" { error("no example selected") } else { example_dir / example / "out" }
exec_path := out_path / example
asm_path := exec_path + ".asm"
object_path := exec_path + ".o"

default:
  @-just example=`ls {{example_dir}} | fzf` run

run: link
  {{exec_path}}

link: build
  @echo Linking {{example}}
  @ld -macosx_version_min 13.0.0 -o {{exec_path}} {{object_path}} -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64
  
build: assemble
  @echo Building {{example}}
  @as {{asm_path}} -o {{object_path}}

assemble:
  @mkdir -p {{out_path}}
  @echo Assembling {{example}}
  @RUSTFLAGS=-Awarnings cargo -q run {{source_path}} -o {{asm_path}}
