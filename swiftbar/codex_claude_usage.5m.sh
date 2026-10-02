#!/usr/bin/env bash
#<xbar.title>Codex + Claude Usage</xbar.title>
#<xbar.version>1.0</xbar.version>
#<xbar.author>OpenAI / adapted from rsnemmen codex-usage-swiftbar</xbar.author>
#<xbar.desc>Display Codex/OpenAI and Claude Code subscription rate-limit utilization</xbar.desc>
#<xbar.dependencies>curl,python3,codex(optional),claude(optional),security(macOS)</xbar.dependencies>

#<xbar.var>boolean(VAR_SHOW_WEEKLY="false"): Also show weekly usage in the menu-bar title.</xbar.var>
#<xbar.var>boolean(VAR_COLORS="true"): Color-code the title at warning/critical levels.</xbar.var>
#<xbar.var>boolean(VAR_SHOW_RESET="true"): Show time-until-reset in the dropdown.</xbar.var>
#<xbar.var>string(VAR_CODEX_SOURCE="auto"): Codex source: auto|oauth|cli</xbar.var>
#<xbar.var>boolean(VAR_SHOW_CLAUDE_SCOPED="true"): Show Claude model-scoped weekly limits when reported.</xbar.var>
#<xbar.var>boolean(VAR_SHOW_LOGO="true"): Draw the title as provider logos with bars instead of text.</xbar.var>

SHOW_WEEKLY="${VAR_SHOW_WEEKLY:-false}"
COLORS="${VAR_COLORS:-true}"
SHOW_RESET="${VAR_SHOW_RESET:-true}"
CODEX_SOURCE="${VAR_CODEX_SOURCE:-auto}"
SHOW_CLAUDE_SCOPED="${VAR_SHOW_CLAUDE_SCOPED:-true}"
SHOW_LOGO="${VAR_SHOW_LOGO:-true}"

CACHE_TTL=300
CODEX_USAGE_CACHE="/tmp/.codex_swiftbar_cache_v2"
CODEX_TOKEN_CACHE="/tmp/.codex_swiftbar_token"
CODEX_TOKEN_TTL=900
CLAUDE_USAGE_CACHE="/tmp/.claude_swiftbar_cache_v1"

python_eval() { python3 - "$@"; }

is_pct() {
  case "$1" in
    [0-9]|[1-9][0-9]|100) return 0 ;;
    *) return 1 ;;
  esac
}

color_for_pct() {
  local pct="$1"
  if [ "$COLORS" = "true" ]; then
    [ "$pct" -ge 90 ] 2>/dev/null && echo "#FF0000" && return
    [ "$pct" -ge 70 ] 2>/dev/null && echo "#FFD700" && return
  fi
  echo ""
}

title_color() {
  local vals=("$@") p c
  for p in "${vals[@]}"; do
    is_pct "$p" || continue
    c="$(color_for_pct "$p")"
    [ "$c" = "#FF0000" ] && echo "#FF0000" && return
  done
  for p in "${vals[@]}"; do
    is_pct "$p" || continue
    c="$(color_for_pct "$p")"
    [ "$c" = "#FFD700" ] && echo "#FFD700" && return
  done
  echo ""
}

make_bar() {
  local pct="${1:-0}" width=20
  python_eval "$pct" "$width" <<'PY'
import sys
p=max(0,min(100,float(sys.argv[1]))); w=int(sys.argv[2])
n=max(0,min(w,int(round(p*w/100))))
print("█"*n+"░"*(w-n))
PY
}

# One dropdown row: label, bar and reset time on a single line. Deliberately
# carries no color=, because SwiftBar turns any colored line into a clickable
# menu item (configureAction: params.hasAction || params.color != nil).
usage_row() {
  local label="$1" pct="$2" reset="$3" line
  if ! is_pct "$pct"; then
    printf "%-${ROW_LABEL_W}s   — | font=Menlo-Bold size=14\n" "${label}:"
    return
  fi
  line="$(printf "%-${ROW_LABEL_W}s %3s%% %s" "${label}:" "$pct" "$(make_bar "$pct")")"
  if [ "$SHOW_RESET" = "true" ] && [ -n "$reset" ]; then
    local left; left="$(time_until "$reset")"
    [ "$left" = "now" ] && line="${line}  resets now" || line="${line}  resets in $left"
  fi
  printf '%s | font=Menlo-Bold size=14\n' "$line"
}

time_until() {
  local ts="$1"
  [ -z "$ts" ] && echo "?" && return
  python_eval "$ts" <<'PY'
from datetime import datetime, timezone
import sys
ts=sys.argv[1]
try:
    if ts.isdigit():
        reset=datetime.fromtimestamp(int(ts), tz=timezone.utc)
    else:
        reset=datetime.fromisoformat(ts.replace("Z","+00:00"))
        if reset.tzinfo is None:
            reset=reset.replace(tzinfo=timezone.utc)
    secs=(reset-datetime.now(timezone.utc)).total_seconds()
    if secs <= 0:
        print("now")
    else:
        d=int(secs//86400); h=int((secs%86400)//3600); m=int((secs%3600)//60)
        print(f"{d}d {h}h" if d else (f"{h}h {m}m" if h else f"{m}m"))
except Exception:
    print("?")
PY
}

# 18x18 alpha masks (base64 raw bytes), rendered from Font Awesome Free brand marks.
# Alpha-only because SwiftBar templateImage draws them as a single-color mask.
OPENAI_ALPHA18="AAAAAANz2v7smhcAAAAAAAAAAAAAAa7AQBIutuji78JUAAAAAAAAXccFAA2I8YsxJ2TlhgAAAAAlzFMAX+auIgAZAwAg7TsAAHjz+DAk9kIAD4750kEAh5gAVeYo3y8o5gJk6bEnZ+mre7cAz1gA3y8o8szOytJCABCO+a0A/g8A3y8o92ECAVvzrSEAL+hJ+xYA3lAo5gAAAADbrPB5AF3Nv2sAaO615gAAAADbMjrtAAv+O+w9ABed9m0EA2f1MiDtAAj9AKH9nhgANMTY08ntMiDtAFLVAK19m+93L7TlXgHdMiDtKORXAIySADLD94YMAEfzLSL783QAAC7wKQAADwAms+RbAEjOIAAAAABy7HIyOZDxhAsABcJhAAAAAAAAQ7Tj2d/BPB5Iw60BAAAAAAAAAAAAAA+K3/XSbQMAAAAA"
CLAUDE_ALPHA18="AAAAAIvnIwAACpMSAAAAAAAAAAAAALr/kgAAUf89AAAAAAAAAAAAAED+8xEAbf8VAEvxWAAAAECKDwCo/38Ag+YAOPb/aAAAAH3/2y8e9vASnLUa5v++AwAAAAWD/PVmhv+GuInE/+UWAAAAAAAAPNn/tvzs0uT//D8AAAAAAAAAAAmO/f//////oDhuncmzk5OBcGBLb/f////////1xo9BTn6Ll6Cosff////8umw3JRIAAAAAAAAnsff////1n9P9//+qAAAAE5L5sbHf////vg4STopnAABW7OlYYPBoxszuu84XAAAAABT1pBQv9VZwqjj8lYzaIAAAAAABABHcmACjkgCR/T9a1wUAAAAAALLBCADWewAK2M4AFQAAAAAAAYwOAA/9YwAAMIMAAAAAAAAAAAAAAA7bNwAAAAAAAAAA"

# Menu-bar icon: each provider as [logo][5h bar / 7d bar]. Empty output means
# no usable percentages, and the caller falls back to the text title.
make_title_icon() {
  python_eval "$OPENAI_ALPHA18" "$CLAUDE_ALPHA18" "$1" "$2" "$3" "$4" <<'PY'
import base64,struct,sys,zlib
S=18; GAP_LOGO=2; BAR_W=32; GAP_GROUP=8; BANDS=((3,8),(10,15))
def pct(v):
    try: return max(0,min(100,int(round(float(v)))))
    except Exception: return None
groups=[]
for blob,p5,p7 in ((sys.argv[1],sys.argv[3],sys.argv[4]),(sys.argv[2],sys.argv[5],sys.argv[6])):
    a,b=pct(p5),pct(p7)
    if a is None and b is None: continue
    groups.append((base64.b64decode(blob),a,b))
if not groups: raise SystemExit(1)
unit=S+GAP_LOGO+BAR_W
W=len(groups)*unit+(len(groups)-1)*GAP_GROUP
rows=[[0]*W for _ in range(S)]
for gi,(logo,p5,p7) in enumerate(groups):
    x0=gi*(unit+GAP_GROUP)
    for y in range(S):
        for x in range(S): rows[y][x0+x]=logo[y*S+x]
    bx=x0+S+GAP_LOGO
    for (y0,y1),p in zip(BANDS,(p5,p7)):
        if p is None: continue
        filled=int(round(p*BAR_W/100))
        for y in range(y0,y1+1):
            for x in range(BAR_W): rows[y][bx+x]=255 if x<filled else 64
def chunk(tag,data):
    c=struct.pack(">I",len(data))+tag+data
    return c+struct.pack(">I",zlib.crc32(c[4:])&0xffffffff)
raw=b"".join(b"\x00"+b"".join(bytes((0,0,0,a)) for a in row) for row in rows)
print(base64.b64encode(b"\x89PNG\r\n\x1a\n"
    +chunk(b"IHDR",struct.pack(">IIBBBBB",W,S,8,6,0,0,0))
    +chunk(b"IDAT",zlib.compress(raw))+chunk(b"IEND",b"")).decode())
PY
}

# ---------------------------
# Codex
# ---------------------------
normalize_codex_source() {
  case "$1" in auto|oauth|cli) echo "$1";; *) echo "auto";; esac
}
CODEX_SOURCE="$(normalize_codex_source "$CODEX_SOURCE")"

load_codex_oauth_token() {
  if [ -f "$CODEX_TOKEN_CACHE" ]; then
    local age
    age=$(( $(date -u +%s) - $(stat -f %m "$CODEX_TOKEN_CACHE" 2>/dev/null || echo 0) ))
    if [ "$age" -lt "$CODEX_TOKEN_TTL" ]; then
      cat "$CODEX_TOKEN_CACHE"
      return 0
    fi
  fi

  local auth_path parsed
  auth_path="$(python_eval <<'PY'
import os, pathlib
home=pathlib.Path(os.environ.get("CODEX_HOME", str(pathlib.Path.home()/".codex")))
print(home/"auth.json")
PY
)"
  [ -f "$auth_path" ] || return 1

  parsed="$(python_eval "$auth_path" <<'PY'
import json, pathlib, sys
raw=json.loads(pathlib.Path(sys.argv[1]).read_text())
tokens=raw.get("tokens",{})
api=(raw.get("OPENAI_API_KEY") or "").strip()
access=(tokens.get("access_token") or tokens.get("accessToken") or "").strip()
acct=(tokens.get("account_id") or tokens.get("accountId") or "").strip()
token=api or access
if not token: raise SystemExit(1)
print(token); print(acct)
PY
)" || return 1
  printf '%s\n' "$parsed" > "$CODEX_TOKEN_CACHE"
  printf '%s\n' "$parsed"
}

fetch_codex_oauth_usage() {
  local ta token account_id base_url usage_url response http_code body
  ta="$(load_codex_oauth_token)" || return 1
  token="$(printf '%s\n' "$ta" | sed -n '1p')"
  account_id="$(printf '%s\n' "$ta" | sed -n '2p')"
  [ -n "$token" ] || return 1

  base_url="https://chatgpt.com/backend-api"
  usage_url="${base_url}/wham/usage"

  if [ -n "$account_id" ]; then
    response="$(curl -s --connect-timeout 7 --max-time 15 -w "\n%{http_code}" \
      -H "Authorization: Bearer ${token}" \
      -H "Accept: application/json" \
      -H "User-Agent: Codex SwiftBar" \
      -H "ChatGPT-Account-Id: ${account_id}" \
      "$usage_url")"
  else
    response="$(curl -s --connect-timeout 7 --max-time 15 -w "\n%{http_code}" \
      -H "Authorization: Bearer ${token}" \
      -H "Accept: application/json" \
      -H "User-Agent: Codex SwiftBar" \
      "$usage_url")"
  fi

  http_code="$(printf '%s\n' "$response" | tail -n 1)"
  body="$(printf '%s\n' "$response" | sed '$d')"
  [ "$http_code" = "401" ] || [ "$http_code" = "403" ] && { rm -f "$CODEX_TOKEN_CACHE"; return 3; }
  [ -n "$http_code" ] && [ "$http_code" -ge 200 ] 2>/dev/null && [ "$http_code" -lt 300 ] 2>/dev/null || return 4

  python_eval "$body" <<'PY'
import json,sys,datetime
d=json.loads(sys.argv[1]); rl=d.get("rate_limit") or {}
ws=[rl.get("primary_window"),rl.get("secondary_window")]
def mk(w):
    if not isinstance(w,dict): return None
    try:
        return {"used":round(float(w["used_percent"])),
                "reset":int(w["reset_at"]),
                "mins":int(w["limit_window_seconds"])//60}
    except Exception: return None
vals=[mk(x) for x in ws]
session=weekly=None
for w in vals:
    if not w: continue
    if w["mins"]==300: session=w
    elif w["mins"]==10080: weekly=w
    elif session is None: session=w
    elif weekly is None: weekly=w
def out(w):
    if not w: return ("NA","")
    iso=datetime.datetime.fromtimestamp(w["reset"],tz=datetime.timezone.utc).isoformat().replace("+00:00","Z")
    return str(w["used"]),iso
u5,r5=out(session); u7,r7=out(weekly)
print(u5); print(u7); print(r5); print(r7)
PY
}

fetch_codex_cli_usage() {
  command -v codex >/dev/null 2>&1 || return 1
  local raw
  raw="$( { printf '/status\n'; sleep 1; } | codex -s read-only -a untrusted 2>/dev/null )"
  [ -n "$raw" ] || return 1
  python_eval "$raw" <<'PY'
import re,sys
t=re.sub(r'\x1b\[[0-9;?]*[A-Za-z]','',sys.argv[1]).replace('\r','\n')
lines=[x.strip() for x in t.splitlines() if x.strip()]
five=next((x for x in lines if re.search(r'5h limit',x,re.I)),"")
week=next((x for x in lines if re.search(r'weekly limit',x,re.I)),"")
def used(x):
    m=re.search(r'(\d+)\s*%\s*left',x,re.I)
    if m:return str(max(0,min(100,100-int(m.group(1)))))
    m=re.search(r'(\d+)\s*%',x); return m.group(1) if m else "NA"
def reset(x):
    m=re.search(r'\(([^()]*)\)',x); return m.group(1).strip() if m else ""
print(used(five)); print(used(week)); print(""); print("")
PY
}

get_codex_usage() {
  local parsed="" age rc
  if [ -f "$CODEX_USAGE_CACHE" ]; then
    age=$(( $(date -u +%s) - $(stat -f %m "$CODEX_USAGE_CACHE" 2>/dev/null || echo 0) ))
    [ "$age" -lt "$CACHE_TTL" ] && parsed="$(cat "$CODEX_USAGE_CACHE" 2>/dev/null)"
  fi
  if [ -z "$parsed" ]; then
    if [ "$CODEX_SOURCE" = "oauth" ] || [ "$CODEX_SOURCE" = "auto" ]; then
      parsed="$(fetch_codex_oauth_usage 2>/dev/null)"; rc=$?
    fi
    if [ -z "$parsed" ] && { [ "$CODEX_SOURCE" = "cli" ] || [ "$CODEX_SOURCE" = "auto" ]; }; then
      parsed="$(fetch_codex_cli_usage 2>/dev/null)" || true
    fi
    [ -n "$parsed" ] && printf '%s\n' "$parsed" > "$CODEX_USAGE_CACHE"
  fi
  printf '%s\n' "$parsed"
}

# ---------------------------
# Claude Code
# ---------------------------
load_claude_oauth_token() {
  # Explicit token wins.
  if [ -n "${CLAUDE_CODE_OAUTH_TOKEN:-}" ]; then
    printf '%s\n' "$CLAUDE_CODE_OAUTH_TOKEN"
    return 0
  fi

  # Default-profile macOS Keychain.
  if command -v /usr/bin/security >/dev/null 2>&1; then
    local kc
    kc="$(/usr/bin/security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null || true)"
    if [ -n "$kc" ]; then
      python_eval "$kc" <<'PY'
import json,sys
try:
    d=json.loads(sys.argv[1])
    t=((d.get("claudeAiOauth") or {}).get("accessToken") or "").strip()
    if t: print(t)
except Exception: pass
PY
      return $?
    fi
  fi

  # File fallback; respects CLAUDE_CONFIG_DIR.
  local cred
  cred="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/.credentials.json"
  [ -f "$cred" ] || return 1
  python_eval "$cred" <<'PY'
import json,pathlib,sys
try:
    d=json.loads(pathlib.Path(sys.argv[1]).read_text())
    t=((d.get("claudeAiOauth") or {}).get("accessToken") or "").strip()
    if not t: raise SystemExit(1)
    print(t)
except Exception:
    raise SystemExit(1)
PY
}

claude_user_agent() {
  local v
  if command -v claude >/dev/null 2>&1; then
    v="$(claude --version 2>/dev/null | head -n1 | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+' | head -n1)"
  fi
  [ -n "$v" ] && echo "claude-code/$v" || echo "claude-code/2.1.0"
}

fetch_claude_usage() {
  local token ua response http_code body
  token="$(load_claude_oauth_token)" || return 1
  [ -n "$token" ] || return 1
  ua="$(claude_user_agent)"

  response="$(curl -s --connect-timeout 7 --max-time 15 -w "\n%{http_code}" \
    -H "Authorization: Bearer ${token}" \
    -H "Accept: application/json" \
    -H "anthropic-beta: oauth-2025-04-20" \
    -H "User-Agent: ${ua}" \
    "https://api.anthropic.com/api/oauth/usage")"

  http_code="$(printf '%s\n' "$response" | tail -n1)"
  body="$(printf '%s\n' "$response" | sed '$d')"
  [ -n "$http_code" ] && [ "$http_code" -ge 200 ] 2>/dev/null && [ "$http_code" -lt 300 ] 2>/dev/null || return 2

  python_eval "$body" <<'PY'
import json,sys
d=json.loads(sys.argv[1])
def one(k):
    w=d.get(k)
    if not isinstance(w,dict): return ("NA","")
    u=w.get("utilization"); r=w.get("resets_at") or ""
    if u is None: return ("NA",str(r))
    try: u=str(round(float(u)))
    except Exception: u="NA"
    return u,str(r)
for k in ("five_hour","seven_day","seven_day_opus","seven_day_sonnet"):
    u,r=one(k); print(u); print(r)
PY
}

get_claude_usage() {
  local parsed="" age status_cache
  status_cache="${CLAUDE_USAGE_STATUS_CACHE:-$HOME/.cache/claude-code/swiftbar-rate-limits.txt}"

  # Preferred source: data emitted by Claude Code itself to the statusLine hook.
  if [ -f "$status_cache" ]; then
    parsed="$(cat "$status_cache" 2>/dev/null)"
  fi

  # Fallback: our own short-lived OAuth cache.
  if [ -z "$parsed" ] && [ -f "$CLAUDE_USAGE_CACHE" ]; then
    age=$(( $(date -u +%s) - $(stat -f %m "$CLAUDE_USAGE_CACHE" 2>/dev/null || echo 0) ))
    [ "$age" -lt "$CACHE_TTL" ] && parsed="$(cat "$CLAUDE_USAGE_CACHE" 2>/dev/null)"
  fi

  # Last resort: direct OAuth fetch.
  if [ -z "$parsed" ]; then
    parsed="$(fetch_claude_usage 2>/dev/null)" || true
    [ -n "$parsed" ] && printf '%s\n' "$parsed" > "$CLAUDE_USAGE_CACHE"
  fi
  printf '%s\n' "$parsed"
}

# ---------------------------
# Read both sources
# ---------------------------
C="$(get_codex_usage)"
CX5="$(printf '%s\n' "$C" | sed -n '1p')"
CX7="$(printf '%s\n' "$C" | sed -n '2p')"
CX5R="$(printf '%s\n' "$C" | sed -n '3p')"
CX7R="$(printf '%s\n' "$C" | sed -n '4p')"

A="$(get_claude_usage)"
CL5="$(printf '%s\n' "$A" | sed -n '1p')"
CL5R="$(printf '%s\n' "$A" | sed -n '2p')"
CL7="$(printf '%s\n' "$A" | sed -n '3p')"
CL7R="$(printf '%s\n' "$A" | sed -n '4p')"
CLO="$(printf '%s\n' "$A" | sed -n '5p')"
CLOR="$(printf '%s\n' "$A" | sed -n '6p')"
CLS="$(printf '%s\n' "$A" | sed -n '7p')"
CLSR="$(printf '%s\n' "$A" | sed -n '8p')"

# ---------------------------
# Menu-bar title
# ---------------------------
parts=()
is_pct "$CX5" && parts+=("CX ${CX5}%")
is_pct "$CL5" && parts+=("CL ${CL5}%")
if [ "$SHOW_WEEKLY" = "true" ]; then
  is_pct "$CX7" && parts+=("CXw ${CX7}%")
  is_pct "$CL7" && parts+=("CLw ${CL7}%")
fi

if [ "${#parts[@]}" -eq 0 ]; then
  echo "Usage ?"
  echo "---"
  echo "No usage data available."
  echo "Codex: run codex and sign in if needed."
  echo "Claude: run claude /login if needed."
  echo "---"
  echo "Refresh | refresh=true"
  exit 0
fi

ICON=""
[ "$SHOW_LOGO" = "true" ] && ICON="$(make_title_icon "$CX5" "$CX7" "$CL5" "$CL7" 2>/dev/null)"

if [ -n "$ICON" ]; then
  echo " | templateImage=${ICON}"
else
  TITLE=""
  for p in "${parts[@]}"; do
    [ -n "$TITLE" ] && TITLE="${TITLE} · "
    TITLE="${TITLE}${p}"
  done
  TC="$(title_color "$CX5" "$CL5" "$CX7" "$CL7")"
  [ -n "$TC" ] && echo "$TITLE | color=$TC" || echo "$TITLE"
fi

# Widen the label column only when the long model-scoped labels actually appear.
ROW_LABEL_W=3
if [ "$SHOW_CLAUDE_SCOPED" = "true" ] && { is_pct "$CLO" || is_pct "$CLS"; }; then
  ROW_LABEL_W=10
fi

echo "---"
echo "Codex | color=#5A5A5A,#B9B9B9"
usage_row "5h" "$CX5" "$CX5R"
usage_row "7d" "$CX7" "$CX7R"
echo "Open Codex usage | href=https://chatgpt.com/codex/settings/usage"

echo "---"
echo "Claude Code | color=#5A5A5A,#B9B9B9"
usage_row "5h" "$CL5" "$CL5R"
usage_row "7d" "$CL7" "$CL7R"

if [ "$SHOW_CLAUDE_SCOPED" = "true" ]; then
  is_pct "$CLO" && usage_row "Opus 7d" "$CLO" "$CLOR"
  is_pct "$CLS" && usage_row "Sonnet 7d" "$CLS" "$CLSR"
fi

echo "---"
echo "Refresh | refresh=true"
echo "OpenAI status | href=https://status.openai.com/"
echo "Anthropic status | href=https://status.anthropic.com/"
