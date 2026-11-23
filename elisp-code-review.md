# Comprehensive Code Review: Emacs Lisp Configuration and Local Modules

- **Primary Configuration**: `emacs.el` (5,285 lines)
- **Local Modules**: 42 loaded or referenced `.el` modules in `C:\Users\dpchi\elisp`
- **Host Runtime**: GNU Emacs 31.1 (Windows 11 x86_64), 116 ELPA packages installed
- **Evaluation Dimensions**: Correctness, Coherence, Documentation & Valid Commenting

---

## 1. Executive Summary

This review provides a comprehensive technical analysis of `emacs.el` and all local Emacs Lisp
modules maintained in `~/elisp`. The configuration reflects a mature, multi-platform development
environment spanning Unix, macOS, and Windows NT, coordinating Language Server Protocol (`eglot`),
code formatting (`apheleia`), on-the-fly linting (`flymake` and `flycheck`), version control
(`magit`, `gerrit`), and custom productivity tooling for Apigee, CitC (Google G3), Gemini AI, Java,
C#, Python, and more.

### Key Evaluation Findings

| Dimension       | Assessment              | Summary of Critical Observations                                                                     |
|:----------------|:------------------------|:-----------------------------------------------------------------------------------------------------|
| **Correctness** | **Defects Found**       | Fatal load-time failure in `apigee/apigee.el`; runtime type crash in `emacs.el` Magit invocation;    |
:                 :                         : literal string `eq` check bug in `goflycheck.el`; invalid 1-argument `looking-back` calls in         :
:                 :                         : `web-mode` and `powershell-mode`; security exposure of plain passwords via process CLI args in       :
:                 :                         : `salted.el`; session-wide TLS downgrade in `dc-cloud.el`; uninitialized `tz` variables in            :
:                 :                         : `dc-timestamp.el`; double-prompting loop and predicate bug in `smarter-compile.el`.                  :
| **Coherence**   | **Needs Consolidation** | Namespace fragmentation across 7+ prefixes (`dc-`, `dpc-`, `dino-`, `dinoch-`, `ctsn/`, `efi/`,      |
:                 :                         : `aip/`); startup deferral violations in `use-package` (`jsonnet-mode`); duplicate library loads      :
:                 :                         : (`dc-sane-sorting`, `dcjava`); mix of deprecated Emacs 19 `defadvice` and modern `advice-add`.       :
| **Commenting**  | **Inconsistent**        | Pervasive Emacs byte-compiler docstring violations (lines > 80 columns, unescaped single quotes);    |
:                 :                         : large blocks of stale dead code; conversational scratchpads and frustration notes; outdated version  :
:                 :                         : references (Emacs 23/24/29 vs current runtime 31.1).                                                 :

---

## 2. Configuration Topology & Architecture

The configuration is structured as a central monolithic orchestrator (`emacs.el`) delegating to modular domain packages:

```
                                  +-------------------+
                                  |     emacs.el      |
                                  |   (5,285 lines)   |
                                  +---------+---------+
                                            |
         +------------------+---------------+----------------+------------------+
         |                  |                                |                  |
         v                  v                                v                  v
  [Core Utilities]   [Language Tooling]             [Cloud / Workflow]   [OS Support]
  - dc-paths.el      - dcjava.el                    - dc-cloud.el        - dpc-sys-windows-nt.el
  - dc-sys.el        - dccsharp.el                  - dc-gemini.el       - dpc-sys-darwin.el
  - dc-edit.el       - csharp-ts-mode-navigation.el - dc-magit-gerrit.el
  - dc-timestamp.el  - dc-python-extensions.el      - dc-g3.el
  - dc-sane-sorting  - goflycheck.el                - apigee/apigee.el
  - smarter-compile  - dpc-jsonnet-mode-fixups.el   - salted.el
                     - web-mode.el / rego-mode.el   - dino-prettierd-cleanup.el
```

Local packages are incorporated using a hybrid of `use-package`, direct `require`, and lazy `autoload`.

---

## 3. Core Configuration Review (`emacs.el`)

### 3.1 Startup Flow & Lazy-Loading Violations
1. **Contradictory Deferral in `jsonnet-mode`** (`emacs.el:1229-1235`):
   ```elisp
   (use-package jsonnet-mode
     :init (progn (require 'flycheck) (require 'eglot))
     :defer t
     :commands (jsonnet-eval-buffer jsonnet-mode jsonnet-reformat-buffer) ...)
   ```
   *Issue*: `:init` blocks execute immediately during Emacs startup before packages are deferred. Eagerly requiring both `flycheck` and `eglot` at startup completely negates `:defer t`, adding significant overhead to the startup timeline.
   *Remediation*: Move package requirements into `:config` or within mode hooks (`dino-jsonnet-mode-fn`).

2. **Package Incoherence & Redundant Path Tests**:

   Across `emacs.el`, there are roughly 15 occurrences checking `(file-exists-p "~/elisp/foo.el")`
   prior to loading (e.g. `peggy-mode`, `rego-mode`, `markdown-mode`, `powershell-mode`, `citc`, `php-mode`).

   While this is syntactically valid and successfully guards against missing files, hardcoding
   the absolute path `~/elisp/...` has two drawbacks:
   - **Portability**: If the repository or elisp directory is relocated (or shared across environments
     with different directory layouts), the hardcoded string check fails even if the directory was
     added to `load-path`.
   - **Idiomatic Alternatives**:
     - For `use-package`: `:if (locate-library "foo")` checks whether the library is resolvable
       anywhere on `load-path`.
     - For standalone `require`: `(require 'foo nil t)` (using the optional 3rd argument `NOERROR = t`)
       returns `nil` safely if the library is not found, avoiding the manual filesystem stat.

### 3.2 Global State & Side Effects
1. **Global Case-Sensitivity Override** (`emacs.el:200`):
   ```elisp
   (setq case-fold-search nil)
   ```

   *Impact*: In Emacs, `case-fold-search` defaults to `t`, enabling smart case-insensitive searches
   and completions unless uppercase characters are typed. Setting this to `nil` globally breaks user
   expectations across search buffers, `isearch`, minibuffer completions, and third-party package
   heuristics. It should be buffer-local or mode-specific.

2. **Window Splitting Disabled Globally** (`emacs.el:196`):
   ```elisp
   (setq split-height-threshold nil
         split-width-threshold nil)
   ```

   *Impact*: This completely prevents `split-window-sensibly` from creating new vertical or
   horizontal splits. Any command invoking `display-buffer` or `pop-to-buffer` (such as `compile`,
   `occur`, or `magit`) is forced to hijack existing windows instead of opening an adjacent split.

3. **Hardcoded Unix `/tmp` on Windows** (`emacs.el:374`):
   ```elisp
   (setq temporary-file-directory "/tmp")
   ```

   *Impact*: On Windows NT, `/tmp` does not exist by default unless mapped to `C:\tmp`. Emacs
   natively identifies `%TEMP%` or `%TMP%` (e.g. `C:/Users/dpchi/AppData/Local/Temp/`). Forcing
   `/tmp` triggers I/O failures in external processes and temporary buffer tools.

4. **Macro-Generated Compiler Warnings from `polymode`** (`emacs.el:340-345`):

   When compiling the `(define-polymode poly-peggy-mode ...)` macro form, `polymode` internally expands to:
   - `make-variable-buffer-local` inside an un-toplevel expression.
   - An obsolete call to `easy-mmode-define-keymap` (deprecated since Emacs 29.1).
   These are not typed in your code directly, but are emitted by the third-party `polymode` package's macro expansion.

### 3.3 Runtime Crash Bug in Magit Setup (`emacs.el:881-888`)
```elisp
(if (file-exists-p (executable-find magit-git-executable))
    (message "found git at %s" magit-git-executable)
  (error (format "git executable (%s) cannot be found" magit-git-executable)))
```

*Bug*: The block intends to verify that Git is installed and provide a helpful error message if it
is missing. However, if Git is not installed or not in `exec-path`, `(executable-find
magit-git-executable)` returns `nil`. Passing `nil` to `(file-exists-p nil)` throws:
`wrong-type-argument stringp nil`

Because `file-exists-p` requires a string argument, it raises an unhandled type error immediately,
preventing the `else` branch with your friendly error message from ever running.

*Remediation*:

```elisp
(if-let* ((git-path (executable-find magit-git-executable)))
    (message "found git at %s" git-path)
  (error (format "git executable (%s) cannot be found" magit-git-executable)))
```

### 3.4 Emacs 31.1 & 32 Deprecations
The compiler warnings reveal several deprecations introduced in Emacs 30-32:
- `flymake-show-diagnostics-at-end-of-line` (`emacs.el:1047`): Obsolete as of 32.1; use `flymake-inline-diagnostics`.
- `eglot-code-action-indications` (`emacs.el:1049`): Obsolete as of 32.1; use `refactor-indications`.
- `eglot-code-actions` (`emacs.el:1036`, `3571`): Obsolete as of 32.1; use `refactor`.
- `eglot-rename` (`emacs.el:1038`): Obsolete as of 32.1; use `refactor-rename`.
- `hs-special-modes-alist` (`emacs.el:2089-2090`, `3431-3438`): Obsolete as of 31.1; Hideshow configuration must now be set via buffer-local variables in mode hooks.
- `easy-mmode-define-keymap` (obsolete as of 29.1): Emitted during the macroexpansion of `(define-polymode poly-peggy-mode ...)` in `polymode`.

---

## 4. In-Depth Module-by-Module Review

### 4.1 Custom Core Libraries

#### `dc-paths.el`
- **Correctness**:
  - `dc-paths/get-env-setting` (line 267): Passes unexpanded `env-filename` to `insert-file-contents` while verifying `expanded-fname`. If the path contains `~`, it fails on some platforms. Also calls `(buffer-string)` twice consecutively.
  - Byte-compiler warnings: assignments and references to free variables `apheleia-mode-alist` and `eglot-server-programs` without prior declaration or conditional guarding.
- **Coherence**: Sound directory deduping and path normalization logic.
- **Commenting**: 4 docstrings exceed 80 characters.

#### `dc-sys.el`
- **Correctness**:
  - `dc-sys/is-directory` (line 57): Inspects `(car (file-attributes dir-name))`. For symbolic links pointing to directories, `car` is the target string, causing the function to report `nil`. Should use `file-directory-p`.
  - `dc-sys/rename-file-and-buffer` (lines 113-125): Passes `(buffer-name)` to `rename-file` instead of `(file-name-nondirectory filename)` or the full path. If `default-directory` differs from the visited file's folder, the file is misplaced or renaming fails. Also sets `set-visited-file-name` with an unexpanded base name.
  - `dc-sys/find-file-with-line-number` (line 129): Around advice on `find-file` parses line and column syntax (`file.js:14:10`). However, executing `(goto-char (point-min))` unconditionally assumes the visited buffer is current.
  - `dc-sys/update-os-window-title` (line 295): Attached to `window-configuration-change-hook`, which fires excessively during minibuffer interaction and window resizing, generating high OSC terminal write frequency.
- **Commenting**: Clean and well structured; some docstrings exceed column width limits.

#### `dc-edit.el`
- **Correctness**:
  - `dc-edit/sort-words-in-region` (lines 351-354):
    ```elisp
    (sorted-words (if case-fold
                      (sort words #'string-lessp)
                    (sort words #'string<)))
    ```
    `string<` is an exact alias for `string-lessp`. Therefore, the `if case-fold` condition executes identical logic regardless of the branch taken. Case insensitivity requires `string-version-lessp` or custom downcased string comparisons.
  - `dc-edit/camel-to-snakecase-word-at-point` (line 331): Emits compiler warning: `` `replace-regexp' is for interactive use only; use `re-search-forward' and `replace-match' instead ``. Also mutates buffer positions during regex replacement without adjusting bounds.
- **Commenting**: Good functional commentary throughout.

#### `dc-timestamp.el`
- **Correctness**:
  - Uninitialized Lexical Variables: In `dc-ts--parse-YYYYMMDDHHMM-time`, `dc-ts--parse-rfc822-time`, `dc-ts--parse-YBe-time`, `dc-ts--parse-HMS-time`, and `dc-ts--parse-YmdHMS-time`, the variable `tz` is declared in `let` bindings without an expression (`(tz)`), generating 5 compiler warnings. Under lexical binding, `tz` evaluates to `nil`, which causes `encode-time` to default to local time.
  - Non-deterministic Seconds Fallback: When parsing strings missing second components, the code uses `(seconds (nth 0 dt))` from `(decode-time (current-time))`, injecting arbitrary real-world seconds into static parsed timestamps.
- **Commenting**: Clear docstrings with regex layout examples.

#### `dc-sane-sorting.el`
- **Correctness**: High algorithmic quality. Successfully creates custom sorting closures for completion tables.
- **Coherence**: Loaded redundantly in `emacs.el` (3 separate `require` calls).

---

### 4.2 Language & Developer Tool Extensions

#### `dcjava.el`
- **Correctness**:
  - Obsolete Dependency: `(require 'cl-seq)` at line 60 is an obsolete Emacs 20-era feature. Should use `(require 'cl-lib)`.
  - TRAMP File Search Mismatch: In `dcjava--command-to-find-java-src`, the code runs `(if (executable-find "rg") ...)`. When accessing remote files over TRAMP, `executable-find` checks the **local** machine, not the remote host, causing `rg` commands to be issued on hosts where ripgrep may not exist.
  - Hardcoded Corporate URL: `dcjava/wacapps-apiplatform-link` hardcodes `https://source.corp.google.com/h/edge-internal/...`.
- **Coherence**: Mix of snake_case argument names (`contained_dir`) with kebab-case elisp conventions. Redundantly required multiple times.
- **Commenting**: Spelling errors in docstrings (`"jut code Java"`, `"geta popup choice"`, `"unweildy"`).

#### `dccsharp.el` & `csharp-ts-mode-navigation.el`
- **Correctness**:
  - `dccsharp/sort-using-statements` (line 124): Uses `beginning-of-buffer`, which is marked for interactive use only (compiler warning; replace with `(goto-char (point-min))`).
  - `dccsharp/auto-add-using` (lines 163, 170): Compiler warnings for undefined functions `eglot-code-actions` and `eglot-execute`.
- **Coherence**: `csharp-ts-mode-navigation.el` is exceptionally well-written, implementing robust AST node traversal upwards to resolve ambiguous parse tree boundaries.

#### `dc-python-extensions.el`
- **Correctness**:
  - Line 610: Reference to free variable `apheleia-formatters`.
  - Line 755: The function `flymake-ruff--run-checker` is not known to be defined. If `flymake-ruff` is not installed or loaded, selecting `ruff` as linter fails.
- **Coherence**: Comprehensive modern Python workflow covering `uv`, `basedpyright`, and `ruff`.

#### `goflycheck.el`
- **Critical Correctness Bug** (`goflycheck.el:55`):
  ```elisp
  (and cmd-string
       (not (eq cmd-string ""))
       ...)
  ```

  *Bug*: `eq` tests for Lisp object identity. Comparing a dynamically read string against the
  literal `""` with `eq` evaluates to `nil` in Emacs Lisp because they are different string
  instances. As a result, non-empty commands can be skipped or misidentified.

  *Remediation*: Replace `(not (eq cmd-string ""))` with `(not (string-empty-p cmd-string))` or `(not (string= cmd-string ""))`.

- **Compiler Warnings**: Assignment to free variable `flycheck-log-level`.

#### `smarter-compile.el`
- **Correctness**:

  - Double Prompt Bug (`smart-compile-select-compile-command`):

    The alist entry `(smart-compile-consider-makefile . nil)` calls
    `smart-compile-consider-makefile`, which prompts the user with `y-or-n-p`. Because `cmd` is
    `nil`, the resolved value logic calls `(or cmd test)`, re-invoking
    `smart-compile-consider-makefile` and prompting the user a **second time**.

  - Logic Flaw in `smart-compile-select-compile-command`:
    ```elisp
    (and (not (symbolp test))
         (not (stringp test))
         (list test)
         (eval test))
    ```
    `(list test)` is ALWAYS truthy because `(list x)` evaluates to a non-empty cons cell `'(x)`. The author intended `(listp test)`.
  - Quoted Lambda: In `smart-compile-consider-projfile`, `'(lambda (elt) ...)` is passed to `smart-compile-any`. In lexical Emacs Lisp, quoted lambdas are not byte-compiled. Should be `#'(lambda ...)`.
- **Commenting**: Contains typos in docstrings (`alits`, `te buffer`, `ther comments`).

#### `dpc-jsonnet-mode-fixups.el`
- **Correctness**: Good TRAMP-aware process handling via `process-file`.
- **Commenting & Hygiene**: Contains 50 lines of commented-out, frustrated trial-and-error code with notes like `"Jeez."` and `"ALL I WANT TO DO IS REDEFINE THE COMMAND... BUT IT SEEMS IMPOSSIBLE."`.

---

### 4.3 Cloud, Platform & Workflow Tools

#### `dc-cloud.el`
- **Security Vulnerability & Correctness**:
  - Session-Wide TLS Downgrade (`dc-cloud.el:111`):
    ```elisp
    (defun dc-cloud/googleapis-token-for-sa (json-keyfile)
      ...
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
      ...)
    ```
    *Impact*: This destructively downgrades TLS across the entire Emacs session, disabling TLS 1.3 for all subsequent HTTPS requests, package downloads, and IRC/IMAP sessions.
  - Command Injection Hazard:
    ```elisp
    (format "node ~/dev/apigee-edge-js-examples/getToken.js -J %s -o %s -v" json-keyfile project-id)
    ```
    Unquoted shell string interpolation without `shell-quote-argument`.

#### `apigee/apigee.el`
- **Critical Load-Time Crash**:
  ```elisp
  (require 'dino-utility)
  (require 'dpc-sane-sorting)
  ```
  *Bug*: `dino-utility` does not exist in the repository or load path. `dpc-sane-sorting` is misspelled (the module is `dc-sane-sorting.el`). Any attempt to `(require 'apigee)` immediately throws `Cannot open load file: No such file or directory`.

#### `salted.el`
- **Security Vulnerability**:
  In `salted-decrypt-buffer`, `salted-encrypt-buffer-to-file`, and `salted-save-buffer-to-new-encrypted-file`:
  ```elisp
  (call-process-region (point-min) (point-max) salted--salt-file-utility
                       t t nil "-in" "-" "-out" "-" "-passphrase" passphrase ...)
  ```
  *Risk*: The passphrase is passed as a command-line argument. On Windows NT and Unix, process arguments are world-readable to other processes running under the same user via `ps`, `Get-CimInstance Win32_Process`, or Task Manager. Passphrases must be passed via standard input or secure IPC.
- **Data Loss Risk**: `(call-process-region ... t t ...)` replaces the active buffer with process output. If the utility fails, the user's buffer is overwritten with error messages.

#### `dino-prettierd-cleanup.el`
- **Performance & Deprecation**:
  - Periodically scans all system processes via a 10-minute timer (`dino/prettierd-cleanup-timer`).
  - On Windows, calls `dino/prettierd--get-command-line` for each Node process, launching `wmic.exe` or `powershell.exe`. Spawning PowerShell processes synchronously inside a loop causes Emacs UI freezes.
  - Furthermore, `wmic.exe` has been officially deprecated and removed from modern Windows 11 builds (24H2+).

#### `dc-gemini.el`
- **Correctness**: Line 226 contains a function name typo: `dc-gemini/chapgpt-shell-converter` ("chapgpt" vs "chatgpt"). Line 235 calls undeclared `chatgpt-shell-google-make-model`.

---

### 4.4 OS Integration Modules

#### `dpc-sys-windows-nt.el`
- **Compiler Warnings**: Assignments to undeclared free variables `archive-zip-use-pkzip`, `archive-zip-extract`, and `explicit-pwsh.exe-args`. Line 52 calls `grep-apply-setting` without `(require 'grep)`.
- **Functionality**: Solid shortpath resolution and PowerShell integration for Windows NT.

#### `dpc-sys-darwin.el`
- Clean macOS-specific integrations (`open-in-finder`, macOS fonts).

---

### 4.5 Vendored and Customized Major/Minor Modes

#### `web-mode.el` (8,400+ lines)
- **Critical Compilation & Runtime Warnings**:
  - `web-mode.el:4370, 5637, 6916, 6918, 7328`: Multiple calls:
    ```elisp
    `looking-back' called with 1 argument, but requires 2 or 3
    ```
    This breaks compatibility with modern Emacs releases where `looking-back` demands an explicit search limit.
  - Obsolete functions: `point-at-bol`, `point-at-eol`, `x-color-values`, and obsolete variable `inhibit-point-motion-hooks`.
  - Empty `defcustom` types: 15 distinct `(list)` without arguments in custom declarations.

#### `powershell-mode.el`
- **Critical Warnings**: Lines 158, 170, 380, 410, 420 call `looking-back` with only 1 argument.
- Undeclared `speedbar-add-supported-extension`.

#### `restclient.el`
- **Correctness & Deprecations**:
  - Line 83 declares a dynamic global variable `success` without any prefix, polluting the global namespace.
  - Lines 91, 99, 105 use obsolete `defadvice` macros.
  - Uses obsolete functions `point-at-bol`, `point-at-eol`, and `url-set-mime-charset-string`.

---

## 5. Correctness Analysis & Bug Catalog

### 5.1 Severity Breakdown

```
CRITICAL (Crashes / Load Failures)
  |-- apigee/apigee.el: Missing 'dino-utility' and wrong filename 'dpc-sane-sorting'
  |-- emacs.el:882: (file-exists-p (executable-find ...)) crashes on nil
  |-- goflycheck.el:55: (eq cmd-string "") object identity check fails
  |-- web-mode.el & powershell-mode.el: looking-back called with 1 argument

HIGH (Security & Data Integrity)
  |-- dc-cloud.el:111: Global TLS 1.3 downgrade via gnutls-algorithm-priority
  |-- salted.el: Plaintext passphrase exposed on command-line args; destructive buffer replace
  |-- dc-sys.el:120: rename-file-and-buffer misplaces files when default-directory differs

MEDIUM (Logic & Performance Bugs)
  |-- smarter-compile.el: Makefile prompt asked twice; (list test) predicate bug
  |-- dc-timestamp.el: Uninitialized tz variable in 5 functions; non-deterministic seconds
  |-- dc-edit.el:351: sort-words-in-region has identical branches for case-fold
  |-- dino-prettierd-cleanup.el: WMIC removal on Win11; O(N) PowerShell process spawning freezes UI
```

---

## 6. Coherence & Design Evaluation

1. **Namespace Fragmentation**:
   The repository lacks an enforced namespace convention:
   - `dc-*` (e.g. `dc-sys`, `dc-edit`, `dc-paths`, `dcjava`, `dccsharp`)
   - `dpc-*` (e.g. `dpc-jsonnet-mode-fixups`, `dpc-sys-windows-nt`)
   - `dino-*` (e.g. `dino-prettierd-cleanup`, `dino-netrc`)
   - Individual sub-prefixes: `ctsn/` (C# treesit nav), `efi/` (elisp fix indent), `aip/` (auto-insert plus).
   *Recommendation*: Standardize on `dc-` or `dpc-` across all personal modules.

2. **Advice System Hygiene**:
   While `dc-sys.el` and `emacs.el` have adopted modern `advice-add`, older modules (`dtrt-indent.el`, `restclient.el`) still rely on obsolete `defadvice`. These should be systematically converted.

3. **Eager vs. Lazy Loading Strategy**:
   `emacs.el` exhibits startup bloat from requiring heavy libraries (`flycheck`, `eglot`, `smarter-compile`, `dcjava`) synchronously during startup. Transitioning all major mode setups to strictly hook-driven `:config` blocks will substantially reduce initial load times.

---

## 7. Commenting & Documentation Quality

1. **Byte-Compiler Standards Violations**:
   Across almost every custom module, docstrings violate GNU Emacs standard guidelines:
   - Lines wider than 80 characters (found in `dc-paths.el`, `dc-sys.el`, `dc-edit.el`, `dc-dired.el`, `rego-mode.el`, `goflycheck.el`).
   - Unescaped single quotes: GNU Emacs documentation markup treats single quotes `'like this'` as symbol links, requiring `\\='` or curved quotes `\`...''`. Unescaped quotes trigger warnings throughout `rego-mode`, `dc-magit-gerrit`, `dtrt-indent`, and `goflycheck`.
2. **Commented-Out Dead Code**:
   Extensive legacy blocks clutter production files:
   - `emacs.el`: Over 500 lines of dead code blocks.
   - `dpc-jsonnet-mode-fixups.el`: 50 lines of abandoned Flycheck macro hacking.
   - `dcjava.el`: Abandoned `x-popup-menu` code.
3. **Tone and Scratchpad Annotations**:
   Personal notes such as `"Jeez."`, `"Inconceivable!"`, and `"Not sure I ever use this at this point"` should be cleaned up or migrated to commit logs.

---

## 8. Prioritized Recommendations & Action Plan

### Tier 1: Immediate Bug Fixes (Zero Regression Risk)
- [ ] **Fix `apigee/apigee.el`**: Remove `(require 'dino-utility)` and correct `(require 'dpc-sane-sorting)` to `(require 'dc-sane-sorting)`.
- [ ] **Fix `emacs.el:882`**: Wrap `(executable-find magit-git-executable)` in `when-let*` before calling `file-exists-p`.
- [ ] **Fix `goflycheck.el:55`**: Change `(not (eq cmd-string ""))` to `(not (string-empty-p cmd-string))`.
- [ ] **Fix `dc-timestamp.el`**: Explicitly initialize `(tz nil)` or remove the unused symbol from `let` bindings.
- [ ] **Fix `smarter-compile.el`**: In alist Makefile rule, supply an explicit handler to eliminate the double-prompt, and change `(list test)` to `(listp test)`.

### Tier 2: Security & Stability Remediation
- [ ] **Harden `salted.el`**: Pass passphrases via standard input or temporary files rather than CLI process arguments. Buffer output should be captured into a temp buffer before replacing the active buffer.
- [ ] **Remove Session TLS Downgrade in `dc-cloud.el`**: Restrict `gnutls-algorithm-priority` to a buffer-local or dynamic `let` binding around specific legacy Apigee Edge requests, never globally.
- [ ] **Fix `dc-sys.el:rename-file-and-buffer`**: Resolve full directory paths with `expand-file-name` before calling `rename-file`.

### Tier 3: Modernization & Emacs 31.1 Compatibility
- [ ] **Patch `web-mode.el` and `powershell-mode.el`**: Provide explicit search limits to all 1-argument `looking-back` calls (e.g. `(looking-back regexp (line-beginning-position))`).
- [ ] **Update Eglot and Hideshow in `emacs.el`**: Migrate `eglot-code-actions` and `eglot-rename` to `refactor` / `refactor-rename`; configure hideshow using buffer-local variables rather than obsolete `hs-special-modes-alist`.
- [ ] **Replace `defadvice`**: Convert remaining `defadvice` invocations in `restclient.el` and `dtrt-indent.el` to `advice-add`.

### Tier 4: Architectural Refinement
- [ ] **Restore Window Splitting**: Remove or adjust `split-height-threshold nil` / `split-width-threshold nil` in `emacs.el` so `display-buffer` behaves predictably.
- [ ] **Scope Case-Sensitivity**: Remove `(setq case-fold-search nil)` from global scope.
- [ ] **Clean Dead Code & Docstrings**: Strip obsolete commented-out blocks and reformat docstrings to comply with the 80-column standard and quote-escaping rules.
