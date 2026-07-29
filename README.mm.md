<p align="center">
  <img width="500" height="281" alt="PooLang banner" src="https://github.com/user-attachments/assets/70181432-f458-4949-beb2-51e6e9eb8549" />
</p>

<h1 align="center">PooLang</h1>

<p align="center">
  <a href="https://www.rust-lang.org">
    <img src="https://img.shields.io/badge/Rust-stable-orange.svg?style=for-the-badge&logo=rust" alt="Rust stable" />
  </a>
  <a href="https://github.com/shayyz-code/poolang/releases/tag/v0.1.11">
    <img src="https://img.shields.io/badge/release-v0.1.11-blue.svg?style=for-the-badge" alt="Release v0.1.11" />
  </a>
  <a href="LICENSE">
    <img src="https://img.shields.io/badge/license-MIT-blue.svg?style=for-the-badge" alt="MIT License" />
  </a>
</p>

<p align="center">
  <a href="https://github.com/shayyz-code/poolang#readme">English</a> |
  <a href="https://github.com/shayyz-code/poolang/blob/main/README.mm.md">မြန်မာ</a>
</p>

PooLang ကို web backend နှင့် infrastructure များအတွက် ရိုးရှင်းသော compiled
language တစ်ခုအဖြစ် တည်ဆောက်ရန် ရည်ရွယ်ထားသည်။ ၎င်း၏ ဦးတည်ချက်သည် Go ကဲ့သို့
ဖြန့်ချိရလွယ်ကူမှု၊ နားလည်ရလွယ်သော syntax နှင့် ownership သို့မဟုတ် lifetime
syntax များကို အသုံးပြုသူထံ မဖော်ပြဘဲ Rust မှ အတုယူထားသော ရှင်းလင်းတိကျမှုတို့ကို
ပေါင်းစပ်ထားသည်။ **Poo (ပူး)** ဟူသောအမည်သည် မြန်မာဘာသာရှိ guinea pig ၏
အမည်မှ ဆင်းသက်လာသည်။

> [!IMPORTANT]
> လက်ရှိ release ဖြစ်သော **v0.1.11** သည် ယခင် tree-walking interpreter
> ဖြစ်သည်။ Roadmap တွင် ဖော်ပြထားသော compiled backend platform ကို v0.2
> အတွက် စီစဉ်ထားပြီး လက်ရှိတွင် မတည်ဆောက်ရသေးပါ။

## ပရောဂျက်အခြေအနေ

| လမ်းကြောင်း | ရရှိနိုင်မှု | ရည်ရွယ်ချက် |
| --- | --- | --- |
| v0.1.11 | အသုံးပြုနိုင် | ယခင် Rust interpreter၊ အလုပ်လုပ်သော examples နှင့် checked library APIs |
| v0.2 | စီစဉ်ထား | Compatibility မယူသော LLVM compiler၊ native runtime၊ package tooling၊ async HTTP/JSON၊ analyzer နှင့် editor support |
| v0.3 | စီစဉ်ထား | Data၊ auth၊ cache၊ queue နှင့် framework tooling ပါဝင်သော opinionated web application platform |
| v0.4 | စီစဉ်ထား | Protobuf/gRPC၊ microservices၊ resilience၊ discovery နှင့် OpenTelemetry |

အောက်တွင် ဖော်ပြထားသော legacy syntax နှင့် v0.2 အကြား compatibility
အာမခံချက် မရှိပါ။ ဆုံးဖြတ်ချက်များ၊ milestone များနှင့် implementation gate
များကို [TODO.md](TODO.md) တွင် ကြည့်နိုင်သည်။

## လက်ရှိ v0.1.11 စွမ်းဆောင်ချက်များ

- Integer၊ float၊ Boolean၊ character၊ string၊ vector နှင့် map value များ။
- Type inference သို့မဟုတ် type အတိအကျပါသော immutable declaration များနှင့် mutable binding များ။
- Arithmetic၊ comparison၊ `and`၊ `or`၊ `not` နှင့် `otherwise` expression များ။
- `if`/`elif`/`else`၊ `while`၊ range/vector `for`၊ `step` နှင့် `return`။
- Typed function များ၊ struct များ၊ instance method များနှင့် legacy struct inheritance။
- Built-in `std`၊ `math` နှင့် `time` module များ။
- `Io`၊ `Parse` နှင့် `Runtime` error များပါသော checked source/file API များ။
- Rust ဖြင့် ရေးသားထားသော lexer၊ parser၊ type-inference pass နှင့် AST-walking interpreter။

`trait` ကို token အဖြစ် သိရှိသော်လည်း ပြည့်စုံသော supported feature
မဟုတ်သေးပါ။ လက်ရှိ PooLang တွင် native compilation၊ HTTP၊ JSON၊ database၊
package၊ gRPC သို့မဟုတ် editor analyzer မပါဝင်သေးပါ။

## ထည့်သွင်းခြင်း

လက်ရှိ v0.1.11 package နှင့် installer များကို macOS၊ Linux နှင့် Windows
အတွက် ထုတ်ဝေထားသည်။

### Homebrew

```bash
brew install shayyz-code/tap/poo
```

### npm

```bash
npm install --global @shayyz-code/poo@latest
```

### Linux သို့မဟုတ် macOS installer

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.sh | sh
```

### Windows PowerShell installer

```powershell
powershell -ExecutionPolicy Bypass -c "irm https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.ps1 | iex"
```

### Source မှ build လုပ်ခြင်း

Stable Rust toolchain ကို ထည့်သွင်းပြီးနောက် အောက်ပါ command များကို
အသုံးပြုပါ။

```bash
git clone https://github.com/shayyz-code/poolang.git
cd poolang
cargo install --path .
```

## Program များကို Run ခြင်း

v0.1 CLI သည် Poo source file path တစ်ခုကို လက်ခံသည်။

```bash
poo path/to/program.poo
```

Repository checkout မှ run လိုပါက:

```bash
cargo run -- examples/app.poo
```

Interpreter ကြာချိန်ကို ပြရန် source path နောက်တွင် `--speed` ထည့်ပါ။

```bash
poo path/to/program.poo --speed
```

Top-level return အောင်မြင်လျှင် `Result: <Value>` ပုံစံဖြင့် ပြသသည်။ File၊
parse နှင့် runtime failure များကို stderr သို့ ရေးပြီး non-zero exit status
ဖြင့် ထွက်သည်။ Legacy CLI တွင် `--help` နှင့် `--version` မပါဝင်သေးပါ။

## Legacy v0.1.11 Syntax

ဤ example ကို လက်ရှိ interpreter ဖြင့် တိုက်ရိုက် run နိုင်သည်။

```poo
use std::poutln;

poo message str = "ready";
mut total <: 0;

for i in 0..5 {
    total = total + i;
}

poof double(value int) >> int {
    return value * 2;
}

if (not false and total > 5) or message == "never" {
    poutln(double(total));
}

return total;
```

မျှော်မှန်းထားသော output:

```text
20
Result: Int(10)
```

Legacy declaration တွင် inferred type အတွက် `<:` ကို အသုံးပြုပြီး explicit
type အတွက် `name type = value` ကို အသုံးပြုသည်။ Mutable declaration သည်
`mut` ဖြင့် စတင်ပြီး reassignment အတွက် ပုံမှန် `=` ကို အသုံးပြုသည်။
Function များတွင် `poof`၊ `value int` ကဲ့သို့ typed parameter နှင့် return
type အတွက် `>>` ကို အသုံးပြုသည်။

## Examples

`examples/app.poo` တွင် collection၊ control flow၊ function၊ struct၊ method၊
module နှင့် inheritance များကို အသုံးပြုထားသည်။

```bash
cargo run -- examples/app.poo
```

`examples/donut.poo` သည် terminal တွင် ဆက်တိုက်လှုပ်ရှားသော donut ကို
ရေးဆွဲပြသသည်။ Terminal ထဲတွင် run ပြီး <kbd>Ctrl</kbd>+<kbd>C</kbd> ဖြင့်
ရပ်ပါ။

```bash
cargo run --release -- examples/donut.poo
```

Example နှစ်ခုလုံးသည် legacy v0.1.11 interpreter အတွက် ဖြစ်သည်။

## Release နှင့် CI

SemVer tag များသည် cargo-dist `Release` workflow ကို စတင်ပေးသည်။ လက်ရှိ
configuration သည် Apple Silicon နှင့် Intel macOS၊ ARM64 နှင့် x86-64 Linux၊
x86-64 Windows တို့အတွက် checksum ပါသော archive များအပြင် shell၊
PowerShell၊ npm၊ Homebrew နှင့် MSI installer များကို ထုတ်ပေးသည်။ Stable
release များသည် npm package နှင့် Homebrew formula update များကို publish
လုပ်သည်။

Pull request များတွင် လက်ရှိ အောက်ပါ check များကို run သည်။

- Issue-link policy check။
- Ubuntu ပေါ်ရှိ Rustfmt၊ Cargo check၊ strict Clippy နှင့် Cargo test suite အပြည့်အစုံ။
- Oranda/mdBook site build နှင့် internal-link check။
- cargo-dist release-plan validation။

`main` သို့ push လုပ်သောအခါ project site ကို ပြန်လည် build လုပ်ပြီး deploy
လုပ်သည်။ Platform release build များသည် pull request တိုင်းအတွက် မဟုတ်ဘဲ
version tag များအတွက်သာ run သည်။

## Development

```bash
cargo build
cargo fmt --all -- --check
cargo check --all-targets
cargo clippy --all-targets --all-features -- -D warnings
cargo test
mdbook build docs
mdbook test docs
```

အဓိက Rust code များသည် `src/` တွင်၊ executable language specification
များသည် `tests/language_specs.rs` တွင်၊ sample program များသည်
`examples/` တွင်၊ documentation source များသည် `docs/src/` တွင်
ရှိသည်။ Generate လုပ်ထားသော `docs/book/` နှင့် `public/` output များကို
Git တွင် မထည့်ထားပါ။

Change တိုင်းအတွက် GitHub issue ကို အရင်ဖွင့်ပြီး `main` မှ branch ခွဲရသည်။
Pull request များကို သေးငယ်စွာ ထားရမည်၊ closing issue reference တစ်ခုတည်း
ပါရမည်၊ Conventional Commits ကို အသုံးပြုရမည်၊ squash merge ဖြင့်သာ
ပေါင်းရမည်။ အသေးစိတ်ကို [CONTRIBUTING.md](CONTRIBUTING.md) တွင် ကြည့်ပါ။

## လိုင်စင်

PooLang ကို [MIT License](LICENSE) ဖြင့် ထုတ်ဝေထားသည်။

<p align="center"><a href="https://github.com/shayyz-code"><b>Shayy</b></a> မှ ဖန်တီးထားသည်</p>
