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

PooLang ကို AI agent များအတွက် sandbox အတွင်း run သော scripting language
တစ်ခုအဖြစ် ပြောင်းလဲတည်ဆောက်နေသည်။ LLM agent က PooLang script ကို ရေးပြီး
host program က ၎င်းကို ဘေးကင်းစွာ run ပေးသည်။ v0.2 မှစ၍ script သည် host က
ခွင့်ပြုထားသော capability နှင့် tool များကိုသာ ခေါ်နိုင်မည်၊ host သတ်မှတ်ထားသော
budget အတွင်းသာ run မည်ဖြစ်ပြီး error များကို agent က ဖတ်ပြီး ပြင်ဆင်နိုင်သော
diagnostic အဖြစ် ပြန်ပေးမည်။ **Poo (ပူး)** ဟူသောအမည်သည် မြန်မာဘာသာရှိ guinea
pig ၏ အမည်မှ ဆင်းသက်လာသည်။

> [!IMPORTANT]
> **အခြေအနေ:** v0.1.11 သည် tree-walking interpreter အသေးတစ်ခုသာ ဖြစ်သည်။
> v0.2 agent sandbox ကို တည်ဆောက်နေဆဲ ဖြစ်သည် —
> [roadmap (TODO.md)](https://github.com/shayyz-code/poolang/blob/main/TODO.md)
> နှင့် [milestones](https://github.com/shayyz-code/poolang/milestones) ကို ကြည့်ပါ။

## PooLang ကို ဘာကြောင့် ဖန်တီးသလဲ

Agent များသည် task များကို code ရေး၍ ဖြေရှင်းလာကြပြီး host သည် ကိုယ်တိုင်
မရေးထားသော၊ အပြည့်အဝ မယုံကြည်နိုင်သော code ကို run ပေးရသည်။ General-purpose
language များသည် file၊ network နှင့် clock တို့ကို အစကတည်းက အသုံးပြုခွင့်ရှိပြီး
နောက်မှ ပိတ်ဆို့ကန့်သတ်ရသည်။ PooLang v0.2 ကိုမူ ဆန့်ကျင်ဘက် ချဉ်းကပ်ပုံဖြင့်
တည်ဆောက်နေသည်။ ဤအရာများ v0.1.11 တွင် မရှိသေးပါ။ v0.2 ၏ ရည်မှန်းချက်များ:

- **Zero ambient authority။** Script သည် host က ခွင့်ပြုထားသော capability
  နှင့် tool များကိုသာ ခေါ်နိုင်မည်။ အခြားမည်သည့်အရာကိုမျှ ထိတွေ့၍ ရမည် မဟုတ်ပါ။
- **Budget များ။** Host သည် step၊ call depth၊ memory နှင့် wall-clock time
  တို့ကို ကန့်သတ်နိုင်မည်ဖြစ်သောကြောင့် ထိန်းမရသော loop သည် agent ကို
  ရပ်တန့်စေမည့်အစား error ဖြင့် အဆုံးသတ်မည်။
- **Determinism။** Script၊ input နှင့် tool result တူလျှင် output လည်း
  တူမည်ဖြစ်သောကြောင့် run များကို ပြန်လည် replay လုပ်နိုင်ပြီး test လုပ်နိုင်မည်။
- **Panic လုံးဝ မဖြစ်ခြင်း။** Input တိုင်းသည် value တစ်ခု သို့မဟုတ် typed
  error တစ်ခုဖြင့် အဆုံးသတ်မည်ဖြစ်ပြီး host ကို crash မဖြစ်စေပါ။
- **LLM က အသုံးချနိုင်သော diagnostic များ။** Error တိုင်းတွင် stable code၊
  source span နှင့် fix hint ပါဝင်မည်ဖြစ်ပြီး JSON အဖြစ်လည်း ရယူနိုင်မည်ဖြစ်သောကြောင့်
  agent သည် မိမိ script ကို ကိုယ်တိုင် ပြင်ဆင်နိုင်မည်။

ပထမဆုံး host interface သည် Rust embedding API ဖြစ်မည်။ Python နှင့် WASM
binding များနှင့် MCP server တို့ကို နောက်ပိုင်းတွင် ထပ်မံ ထည့်သွင်းမည်။

## ပရောဂျက်အခြေအနေ

| လမ်းကြောင်း | ရရှိနိုင်မှု | ရည်ရွယ်ချက် |
| --- | --- | --- |
| v0.1.11 | အသုံးပြုနိုင် | Legacy syntax၊ အလုပ်လုပ်သော examples နှင့် checked library APIs ပါသော Rust tree-walking interpreter အသေး၊ sandbox control မပါ |
| v0.2 | တည်ဆောက်ဆဲ | Agent sandbox: conventional syntax၊ host က ခွင့်ပြုသော capability နှင့် tool များ၊ budget များ၊ deterministic execution၊ JSON diagnostic များ၊ Rust embedding API နှင့် `poo run`/`poo check` |
| နောက်ပိုင်း | အချိန်မသတ်မှတ်ရသေး | Static checker၊ Python နှင့် WASM binding များ၊ MCP server နှင့် editor support |

အောက်တွင် ဖော်ပြထားသော legacy syntax နှင့် v0.2 အကြား compatibility
အာမခံချက် မရှိဘဲ v0.2 တွင် legacy mode လည်း ပါဝင်မည် မဟုတ်ပါ။ အတည်ပြုပြီးသော
ဆုံးဖြတ်ချက်များနှင့် phase များကို
[TODO.md](https://github.com/shayyz-code/poolang/blob/main/TODO.md) တွင်၊
တိုးတက်မှုကို [milestones](https://github.com/shayyz-code/poolang/milestones)
တွင် ကြည့်နိုင်သည်။

## v0.2 Preview

> [!NOTE]
> ဤ syntax ကို **စီစဉ်ထားဆဲဖြစ်ပြီး မတည်ဆောက်ရသေးပါ**။ v0.1.11 က ၎င်းကို
> လက်မခံပါ။

```text
use tools::search;            // host-granted tool
poo limit = 10;
poo mut hits: vec[str] = [];
for doc in search("rust sandbox", limit) {
    if doc["score"] >= 0.5 && doc["title"] != "" { hits.push(doc["title"]); }
}
println(hits.len());
```

v0.2 တွင် host က `search` ကို tool အဖြစ် register လုပ်မှသာ ၎င်းကို
အသုံးပြုနိုင်မည်။ ထိုခွင့်ပြုချက် မရှိလျှင် script ကို run မလုပ်မီကပင်
capability (E06) diagnostic ဖြင့် ငြင်းပယ်သည်။

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
မဟုတ်သေးပါ။ v0.1.11 တွင် အထက်တွင် ဖော်ပြခဲ့သော capability gating၊ budget
နှင့် JSON diagnostic ကဲ့သို့သော sandbox control များ မပါဝင်သေးပါ။ Input
အချို့က interpreter ကို crash ဖြစ်စေနိုင်သေးသောကြောင့် ယုံကြည်စိတ်ချ၍ မရသော
code ကို run ရန် မသုံးပါနှင့်။ သိရှိထားသော ချို့ယွင်းချက်များကို roadmap ၏
[current audit](https://github.com/shayyz-code/poolang/blob/main/TODO.md#current-audit)
တွင် ဖော်ပြထားသည်။

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
- Commit policy check: Conventional Commit header များနှင့် commit တစ်ခုစီတွင်
  Cargo check နှင့် test။
- Ubuntu ပေါ်ရှိ Rustfmt၊ Cargo check၊ strict Clippy နှင့် Cargo test suite အပြည့်အစုံ။
- mdBook build နှင့် test များ၊ Oranda/mdBook site build နှင့် internal-link check။
- Generate လုပ်ထားသော release workflow သည် `dist-workspace.toml` နှင့်
  ကိုက်ညီမှု ရှိမရှိ စစ်ဆေးခြင်း။
- Dependency policy အတွက် `cargo deny check`။
- cargo-dist release-plan run (`dist plan`)။

`main` သို့ push လုပ်သောအခါ project site ကို ပြန်လည် build လုပ်ပြီး deploy
လုပ်သည်။ Platform release build များသည် pull request တိုင်းအတွက် မဟုတ်ဘဲ
version tag များအတွက်သာ run သည်။

## Development

```bash
just setup       # enable the commit-msg hook (once per clone)
just check-fast  # formatting, compilation, Clippy, and tests
just check       # every check required before review
```

`just check` သည် mdBook documentation ကို build နှင့် test လုပ်ခြင်း၊
generate လုပ်ထားသော release workflow ကို စစ်ဆေးခြင်းနှင့် `cargo deny check`
ကို run ခြင်းတို့ကိုပါ ပြုလုပ်သည်။ လိုအပ်သော tool များနှင့် version များကို
[CONTRIBUTING.md](CONTRIBUTING.md) တွင် ကြည့်ပါ။

အဓိက Rust code များသည် `src/` တွင်၊ executable language specification
များသည် `tests/language_specs/` တွင်၊ sample program များသည်
`examples/` တွင်၊ documentation source များသည် `docs/src/` တွင်
ရှိသည်။ Generate လုပ်ထားသော `docs/book/` နှင့် `public/` output များကို
Git တွင် မထည့်ထားပါ။

Change တိုင်းအတွက် GitHub issue ကို အရင်ဖွင့်ပြီး `main` မှ branch ခွဲရသည်။
Pull request များကို သေးငယ်စွာ ထားရမည်၊ closing issue reference တစ်ခုတည်း
ပါရမည်၊ rebase merge ဖြင့်သာ ပေါင်းရမည်။ Commit တိုင်းသည် `main` ပေါ်သို့
ရေးထားသည့်အတိုင်း ရောက်သွားသောကြောင့် commit တစ်ခုစီသည် `just check` ကို
pass ဖြစ်သော self-contained Conventional Commit ဖြစ်ရမည်။ အသေးစိတ်ကို
[CONTRIBUTING.md](CONTRIBUTING.md) တွင် ကြည့်ပါ။

## လိုင်စင်

PooLang ကို [MIT License](LICENSE) ဖြင့် ထုတ်ဝေထားသည်။

<p align="center"><a href="https://github.com/shayyz-code"><b>Shayy</b></a> မှ ဖန်တီးထားသည်</p>
