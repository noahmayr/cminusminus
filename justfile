example_dir := "./examples"
example := "helloworld"
entry := "main.cmm"
source_path := example_dir / example / entry
out_path := if example == "" { error("no example selected") } else { example_dir / example / "out" }
exec_path := out_path / example
asm_path := exec_path + ".s"
ll_path := exec_path + ".ll"
object_path := exec_path + ".o"

default:
  @-just example=`ls {{example_dir}} | fzf` run

build_run: compile lower assemble link exec

exec:
  {{exec_path}}

link:
  @echo Linking {{example}}
  @ld64.lld -arch arm64 -platform_version macos 13.2.0 13.2.0 -lSystem -L`xcrun --sdk macosx --show-sdk-path`/usr/lib {{object_path}} -o {{exec_path}}

assemble:
  @echo Assembling {{example}}
  @as {{asm_path}} -o {{object_path}}

lower:
  @echo Lowering {{example}}
  @llc --aarch64-neon-syntax=apple {{ll_path}} -o {{asm_path}} 

compile:
  @mkdir -p {{out_path}}
  @echo Compiling {{example}}
  @cargo run {{source_path}} -o {{ll_path}}

run:
  @cargo run {{source_path}} -x
