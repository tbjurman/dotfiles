##############################################################################
# aliases
##############################################################################
alias pb='ps -ef | grep beam.smp | grep -v grep'
alias kb='killall -9 beam.smp'
alias ver='. ~/bin/version.sh'
alias vim='nvim'

##############################################################################
# environment
##############################################################################
set -xg MANPATH /usr/share/man $MANPATH
set -xg WEBUI_SKIP true
# this means don't build OTP in fish
set -xg TYPE debug

##############################################################################
# fish git prompt
##############################################################################
set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)

set __fish_git_prompt_showdirtystate 'yes'
# set __fish_git_prompt_showstashstate 'yes'
# set __fish_git_prompt_showuntrackedfiles 'yes'
# set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch green
# set __fish_git_prompt_color_upstream_ahead green
# set __fish_git_prompt_color_upstream_behind red

set __fish_git_prompt_char_dirtystate '*'
# set __fish_git_prompt_char_dirtystate '⚡'
# set __fish_git_prompt_char_stagedstate '→'
# set __fish_git_prompt_char_untrackedfiles '☡'
# set __fish_git_prompt_char_stashstate '↩'
# set __fish_git_prompt_char_upstream_ahead '+'
# set __fish_git_prompt_char_upstream_behind '-'

function fish_prompt
    set last_status $status

    printf '%s' (__fish_git_prompt)

    set_color $fish_color_cwd
    printf ' %s $ ' (prompt_pwd)
    set_color normal

    set_color normal
end

##############################################################################
# fish coloring
##############################################################################
# (for light theme) set fish_color_cwd blue
set fish_color_cwd cyan
set fish_color_operator yellow
set fish_color_escape yellow
set fish_color_command yellow # for dark theme
# (for light theme) set fish_color_autosuggestion 999

# opam configuration
source /home/tbjurman/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
