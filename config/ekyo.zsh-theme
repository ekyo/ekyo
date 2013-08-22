
PROMPT='%{$fg[cyan]%}え京 %{$reset_color%}'

RPROMPT='%{$fg_bold[magenta]%}%c%{$reset_color%}%{$fg[blue]%}$(git_prompt_info)$(git_time_since_commit)%{$reset_color%}$(git_prompt_status)%{$reset_color%} ${time}%{$reset_color%}'

# local time, color coded by last return code
time_enabled="%(?.%{$fg[green]%}.%{$fg[red]%})%*%{$reset_color%}"
time_disabled="%{$fg[green]%}%*%{$reset_color%}"
time=$time_enabled

ZSH_THEME_GIT_PROMPT_PREFIX=" git:%{$fg[cyan]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[yellow]%} ✗" # Ⓓ
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[cyan]%} ✭" # ⓣ
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%} ☀" # Ⓞ

ZSH_THEME_GIT_PROMPT_ADDED="%{$fg_bold[cyan]%} ✚" # ⓐ ⑃
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%} ⚡"  # ⓜ ⑁
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg_bold[red]%} ✖" # ⓧ ⑂
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg_bold[blue]%} ➜" # ⓡ ⑄
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg_bold[magenta]%} ♒" # ⓤ ⑊

# More symbols to choose from:
# ☀ ✹ ☄ ♆ ♀ ♁ ♐ ♇ ♈ ♉ ♚ ♛ ♜ ♝ ♞ ♟ ♠ ♣ ⚢ ⚲ ⚳ ⚴ ⚥ ⚤ ⚦ ⚒ ⚑ ⚐ ♺ ♻ ♼
# ✡ ✔ ✖ ✚ ✱ ✤ ✦ ❤ ➜ ➟ ➼ ✂ ✎ ✐ ⨀ ⨁ ⨂ ⨍ ⨎ ⨏
# ⬅ ⬆ ⬇ ⬈ ⬉ ⬊ ⬋ ⬒ ⬓ ⬔ ⬕ ⬖ ⬗ ⬘ ⬙ ⬟  ⬤ 〒 ǀ ǁ ǂ ĭ Ť Ŧ


# Determine the time since last commit. If branch is clean,
# use a neutral color, otherwise colors will vary according to time.
function git_time_since_commit() {
    if git rev-parse --git-dir > /dev/null 2>&1; then
        # Only proceed if there is actually a commit.
        if [[ $(git log 2>&1 > /dev/null | grep -c "^fatal: bad default revision") == 0 ]]; then
            # Get the last commit.
            last_commit=`git log --pretty=format:'%at' -1 2> /dev/null`
            now=`date +%s`
            seconds_since_last_commit=$((now-last_commit))

            # Totals
            MINUTES=$((seconds_since_last_commit / 60))
            HOURS=$((seconds_since_last_commit/3600))

            # Sub-hours and sub-minutes
            DAYS=$((seconds_since_last_commit / 86400))
            SUB_HOURS=$((HOURS % 24))
            SUB_MINUTES=$((MINUTES % 60))

            if [[ -n $(git status -s 2> /dev/null) ]]; then
                if [ "$MINUTES" -gt 30 ]; then
                    COLOR="$ZSH_THEME_GIT_TIME_SINCE_COMMIT_LONG"
                elif [ "$MINUTES" -gt 10 ]; then
                    COLOR="$ZSH_THEME_GIT_TIME_SHORT_COMMIT_MEDIUM"
                else
                    COLOR="$ZSH_THEME_GIT_TIME_SINCE_COMMIT_SHORT"
                fi
            else
                COLOR="$ZSH_THEME_GIT_TIME_SINCE_COMMIT_NEUTRAL"
            fi

            if [ "$HOURS" -gt 24 ]; then
                echo " $COLOR${DAYS}d${SUB_HOURS}h${SUB_MINUTES}m%{$reset_color%}"
            elif [ "$MINUTES" -gt 60 ]; then
                echo " $COLOR${HOURS}h${SUB_MINUTES}m%{$reset_color%}"
            else
                echo " $COLOR${MINUTES}m%{$reset_color%}"
            fi
        else
            COLOR="$ZSH_THEME_GIT_TIME_SINCE_COMMIT_NEUTRAL"
            echo " $COLOR~"
        fi
    fi
}