## Following settings should be shared by both bash and zsh.
#environement variables

die() {
    echo "DIE: $@"
    if [[ "$-" =~ i ]]; then
        return 1
    else
        exit 1
    fi
}

function emacs_edit ()
{
    local fn=
    local ln=
    local cn=
    local args=

    while [ $# -ne 0 ] ; do
        if [[ "$1" == -* ]]; then
            if [ -z "$args" ]; then
                args="$1"
            else
                args="$args $1"
            fi
        else
            fn=`echo "$1" | awk -F ":" '{print $1}'`
            ln=`echo "$1" | awk -F ":" '{print $2}'`
            cn=`echo "$1" | awk -F ":" '{print $3}'`
            break
        fi
        shift 1
    done

    if [ -z $ln ]; then
        run-emacs $args $fn
    else
        if [ -z "$cn" ]; then
            run-emacs $args "+$ln" $fn
        else
            run-emacs $args "+${ln}:${cn}" $fn
        fi
    fi
}

alias ee=emacs_edit
alias eet="emacs_edit -t"
alias edit=emacs_eidt



### following functions requires fzf to be installed..

# wrapper of fzf.
export FZF_DEFAULT_OPTS='--height 60% --border'
FZF_PREVIEW_OPTS='bat --color "always" {} || head -n 500 {}'
function fzp ()
{
    which fzf >/dev/null || die "fzf not installed..."
    fzf --preview  ${FZF_PREVIEW_OPTS}
}

function my_fzf ()
{
    if [ $# -eq 0 ]; then
        fzf --preview  ${FZF_PREVIEW_OPTS}
    elif [ $# -eq 1 ]; then
        find  . -name $1 | fzf --preview  ${FZF_PREVIEW_OPTS}
    else
        find  . $@ | fzf --preview  ${FZF_PREVIEW_OPTS}
    fi
}



# fzf + editor
function open_after_fzf ()
{
    [ $# -ge 1 ] || die "usage: edit_fzf editor [args]"
    which $1 >/dev/null 2>&1 || die "$1 not found."

    local app=$1
    shift 1
    file=`my_fzf $@`

    if [ -n "$file" ]; then
        $app $file
    fi
}

# fzf + mpv
function fzm ()
{
    open_after_fzf mpv $@
}

function eef ()
{
    open_after_fzf emacs_edit $@
}

function vif ()
{
    open_after_fzf vi $@
}

alias fze=eef
alias fzv=vif

# fzf + ssh

function fzf-ssh () {
    local selected_host=$(
        command /bin/cat <(/bin/cat ~/.ssh/config /etc/ssh/ssh_config 2> /dev/null | \
                               command grep -i '^host ' | \
                               command grep -v '[*?]' | \
                               awk '{for (i = 2; i <= NF; i++) print $1 " " $i}') \
                <(command grep -oE '^[[a-z0-9.,:-]+' ~/.ssh/known_hosts | tr ',' '\n' | \
                      tr -d '[' | \awk '{ print $1 " " $1 }') \
                <(command grep -v '^\s*\(#\|$\)' /etc/hosts | \
                      command grep -Fv '0.0.0.0') | \
            awk '{if (length($2) > 0) {print $2}}' | sort -u | fzf --query "$LBUFFER" --prompt="SSH Remote > ")

    if [ -n "$selected_host" ]; then
        BUFFER="ssh ${selected_host}"
        zle accept-line
    fi
    zle reset-prompt
}

# function to call valgrind and show output...
function tval ()
{
    if [ $# -lt 1 ]; then
        cat <<EOF
Usage: tval EXECUTABLE [args]
EOF
        return 0
    fi

    app=`basename $1`
    tmpfile=$(mktemp --suffix=".log" valgrind_"$app"_XXXXXXXX)
    echo "Will write to file: $tmpfile"

    valgrind  --leak-check=full --undef-value-errors=no \
              --log-fd=1 --log-file=$tmpfile "$@" &

    tail -f $tmpfile
}

function tperf-record()
{
    if [ -e perf.data ]; then
        sudo mv perf.data "perf_`date +'%m_%d_%H:%M:%S'`.data"
    fi

    sudo perf record \
         -e cycles,instructions,branch-misses,cache-misses \
         $*
}


#mkdir and cd
function mcd ()
{
    mkdir $1 && cd $1
}


function svnedit ()
{
    if [ $# -lt 2 ]; then
        echo "Usage: svnedit revision URL"
        return
    fi

    svn propedit -r $1 --revprop svn:log $2
}


function o_dump_addr ()
{
    local exe=
    local before=0
    local after=

    while getopts e:a:b var; do
        case $var in
            e)
                exe="$OPTARG"
                if [ ! -f $exe ]; then
                    printf "file $exe does not exist\n"
                    return 2
                fi
                ;;
            a)
                after="$OPTARG"
                ;;
            b)
                before="$OPTARG"
                ;;
            ?)
            printf "Usage: o_dump_addr -e binary addr[addr...]\n"
            return 2
            ;;
        esac
    done
    shift $(($OPTIND - 1))

    if [ $# -eq 0 ]; then
        printf "\nMissing addresses, showing help:\n"
        printf "Usage: o_dump_addr -e binary addr[addr...]\n"
        return 2
    fi

    for addr in $*; do
        if [ $(expr $addr : "0x[0-9a-fA-F]\+$") -eq 0 ]; then
            if [ $(expr $addr : "[0-9a-fA-F]\+$") -eq 0 ]; then
                printf "Warning: %s is not valid address, skipped...\n" $addr
                continue
            else
                addr="0x$addr"
            fi
        fi

        start_address=$addr
        new_addr=$(($addr - $before))
        if [ $new_addr -gt 0 ]; then
            start_address=$new_addr
        fi

        if [ -z $after ]; then
            objdump -d -S --start-address=$start_address $exe | awk '{print $0} $3~/retq?/{exit}'
        else
            if [ $after -eq 0 ]; then
                after=1
            fi
            end_address=$(($addr + $after))
            objdump -d -S --start-address=$start_address --stop-address=$end_address $exe | \
                grep -v 'elf64-'
        fi
    done
}

function o_dump_exe ()
{
    if [ $# -ne 1 ]; then
        die "usage: o_dump_exe exe"
    fi

    objdump -j .text -d $1 | \
        sed -r "s/^[[:blank:]]+[[:xdigit:]]+:[[:blank:]][ a-z0-9]+[[:blank:]]/\t/g" | \
        sed -r "s/[[:xdigit:]]+[[:blank:]]+</</g" | \
        sed -r "s/0x[[:xdigit:]]+(\(%.*?)[[:blank:]]+# (<.*?>)/\2\1/g" |\
        sed -r "s/0x[[:xdigit:]]+/VAL/g" |\
        > $1.s
    echo "Dumpped to: $1.s"
}

function g_dump_symbol ()
{
    if [ $# -lt 2 ]; then
        printf "Usage: g_dump_symbol binary symbol[symbol...]\n"
        return
    fi

    gdb --version > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        printf "g_dump_symbol requires gdb which is not available...\n"
        return
    fi

    exe=$1
    if [ ! -f $exe ]; then
        printf "file $exe does not exist\n"
        return
    fi

    shift

    for symbol in $*; do
        gdb -batch -ex "file $exe" -ex "disassemble $symbol" | sed -r "s/0x0+/0x/g"
    done
}


# git related functions.
function gtop ()
{
    dir=`git rev-parse --show-toplevel`
    cd $dir
}

function kill_all ()
{
    if [ $# -lt 1 ]; then
        echo "Usage: kill_all app [apps]"
        return 1
    fi

    fn=`mktemp -u -t kill_all_XXX.sh`
    for app in $*; do
        cat <<EOF > $fn
#!/bin/bash
ps aux | grep '$app' | grep -v grep | awk -F " " '{print \$2}' | xargs kill -9 {} \;
EOF
        bash $fn
    done
    rm -rf $fn
}

# ssh related.
function get_ssh_target ()
{
    local target=$1
    local pattern='^[0-9.]+$'
    if [[ $target =~ $pattern ]]; then
        if [ -z $(expr $1 : "\(.*\?\..*\?\..*\?\.\)") ]; then # not xx.xx.xx.xx
            if [ ! -z $(expr $1 : "\(.*\?\..*\?.*\?\.\)") ]; then # xx.xx.xx, wtf? not support.
                echo "WTF"
            else
                if [ ! -z $(expr $1 : "\(.*\?\..*\?\)") ]; then # xx.xx, add 192.168
                    target="192.168.$1"
                else #
                    target="192.168.103.$1"
                fi
            fi
        fi
    fi
    echo $target
}


function calc_port ()
{
    local port=`echo $1 |  awk 'BEGIN { FS = "." }
{
     total=0
     for (i = 1; i <= NF; i++) {
         total+= $i * ( 2 ^ (14 - i))
     }
    print total
}'`
    port=$((port+$$+$RANDOM))
    port=$((port%65536))
    if [ $port -lt 2000 ]; then
        port=$((port+2000))
    fi
    echo $port
}

function ash ()
{
    if [ $# -ne 2 ]; then
        cat <<EOF
Usage: ash USER HOST
EOF
        return 1
    fi
    local port_next=`calc_port $2`
    local step=$(($$%5+2))
    while [ 1 ] ; do
        port=$port_next
        echo "Connecting with command: autossh -M $port $1@$2"
        autossh -M $port $1@$2
        if [ $? -ne 0 ]; then
            port_next=$((port+step))
            echo "Failed to connect to $1@$2 with port: $port, try next port $port_next"
            sleep 0.2
        else
            break
        fi
    done
}

function osh ()
{
    echo "Install autossh and set SSH to ash for better experiences..."

    if [ $# -ne 2 ]; then
        cat <<EOF
Usage: osh USER HOST
EOF
        return 1
    fi

    ssh -l $1 $2
}

SSH=osh

which autossh > /dev/null 2>&1
if [ $? -eq 0 ]; then
    SSH=ash
fi

function ysh ()
{
    $SSH  yyc $(get_ssh_target $1)
}

function rsh ()
{
    $SSH  root $(get_ssh_target $1)
}

function msh ()
{
    $SSH  ${USER} $(get_ssh_target $1)
}

# USAGE: $SSH user host


__assert_sigpipe_ok() {
    # When extracting a tar file like this:
    #
    #     bzip2 -dc foo.tar.bz2 | tar xof -
    #
    # For some tar files (see bug #309001), tar will
    # close its stdin pipe when the decompressor still has
    # remaining data to be written to its stdout pipe. This
    # causes the decompressor to be killed by SIGPIPE. In
    # this case, we want to ignore pipe writers killed by
    # SIGPIPE, and trust the exit status of tar. We refer
    # to the bash manual section "3.7.5 Exit Status"
    # which says, "When a command terminates on a fatal
    # signal whose number is N, Bash uses the value 128+N
    # as the exit status."

    local x pipestatus=${PIPESTATUS[*]}
    for x in $pipestatus ; do
        # Allow SIGPIPE through (128 + 13)
        if [[ $x -ne 0 && $x -ne ${PORTAGE_SIGPIPE_STATUS:-141} ]]
        then
            echo "$@"
            return 1
        fi
    done

    # Require normal success for the last process (tar).
    if [[ $x -ne 0 ]]; then
        echo "$@"
        return 1
    fi
}

unpackfile() {
    local x
    local y y_insensitive
    local suffix suffix_insensitive
    local myfail
    local eapi=${EAPI:-0}
    [ -z "$*" ] && echo "Nothing passed to the 'unpack' command" && return 1

    for x in "$@"; do
        echo ">>> Unpacking ${x} to ${PWD}"
        suffix=${x##*.}
        suffix_insensitive=$(LC_ALL=C tr "[:upper:]" "[:lower:]" <<< "${suffix}")
        y=${x%.*}
        y=${y##*.}
        y_insensitive=$(LC_ALL=C tr "[:upper:]" "[:lower:]" <<< "${y}")

        if [[ ! -s ${x} ]]; then
            echo "unpack: ${x} does not exist"
            return 1
        fi

        __unpack_tar() {
            $* -c -- $x | tar xovf -
            __assert_sigpipe_ok "$myfail" || return 1
        }

        myfail="unpack: failure unpacking ${x}"
        case "${suffix_insensitive}" in
            tar)
                if ! tar xof "$x"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            tgz)
                if ! tar xozf "$x"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            tbz|tbz2)
                __unpack_tar bzip2 -d | return 1
                __assert_sigpipe_ok "$myfail" || return 1
                ;;
            zip|jar)
                # unzip will interactively prompt under some error conditions,
                # as reported in bug #336285
                if ! unzip -qo "${x}"; then
                    echo "$myfail"
                    return 1
                fi < <(set +x ; while true ; do echo n || break ; done)
                ;;
            gz|z)
                __unpack_tar gzip -d || return 1
                ;;
            bz2|bz)
                __unpack_tar bzip2 -d || return 1
                ;;
            7z)
                local my_output
                my_output="$(7z x -y "${x}")"
                if [ $? -ne 0 ]; then
                    echo "${my_output}" >&2
                    die "$myfail"
                fi
                ;;
            rar)
                if ! unrar x -idq -o+ "${x}"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            lha|lzh)
                if ! lha xfq "${x}"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            a)
                if ! ar x "${x}"; then
                    echo "$myfail"
                    return 1
                fi
                ;;
            deb)
                # Unpacking .deb archives can not always be done with
                # `ar`.  For instance on AIX this doesn't work out.
                # If `ar` is not the GNU binutils version and we have
                # `deb2targz` installed, prefer it over `ar` for that
                # reason.  We just make sure on AIX `deb2targz` is
                # installed.
                if [[ $(ar --version 2>/dev/null) != "GNU ar"* ]] && \
                       type -P deb2targz > /dev/null; then
                    y=${x##*/}
                    local created_symlink=0
                    if [ ! "$x" -ef "$y" ] ; then
                        # deb2targz always extracts into the same directory as
                        # the source file, so create a symlink in the current
                        # working directory if necessary.
                        if ! ln -sf "$x" "$y"; then
                            echo "$myfail"
                            return 1
                        fi
                        created_symlink=1
                    fi
                    if ! deb2targz "$y"; then
                        echo "$myfail"
                        return 1
                    fi
                    if [ $created_symlink = 1 ] ; then
                        # Clean up the symlink so the ebuild
                        # doesn't inadvertently install it.
                        rm -f "$y"
                    fi
                    if ! mv -f "${y%.deb}".tar.gz data.tar.gz; then
                        if ! mv -f "${y%.deb}".tar.xz data.tar.xz; then
                            echo "$myfail"
                            return 1
                        fi
                    fi
                else
                    if ! ar x "$x"; then
                        echo "$myfail"
                        return 1
                    fi
                fi
                ;;
            lzma)
                __unpack_tar lzma -d || return 1
                ;;
            xz)
                __unpack_tar xz  -d || return 1
                ;;
            txz)
                __unpack_tar xz -d || return 1
                ;;
            *)
                echo "unpack ${x}: file format not recognized. Ignoring."
                ;;
        esac
    done
}

alias up="unpackfile"
alias rget="wget -c -r -np -k -L -p"
alias wget="wget -c"

function eget ()
{
    __get_item() {
        wget -c "https://mirrors.tuna.tsinghua.edu.cn/gentoo/distfiles/$1" || \
        wget -c "http://mirrors.163.com/gentoo/distfiles/$1"
    }

    local failed_list=
    for item in $*; do
        ok=
        __get_item $item
        if [ $? -eq 0 ]; then
            ok=1
        else
            for suffix in "tar.gz" "tar.bz2" "tar.xz" "tgz" "zip"; do
                $fn=
                __get_item "$item.$suffix"
                if [ $? -eq 0 ]; then
                    ok=1
                    break
                fi
            done
        fi

        if [ -z $ok ]; then
            failed_list="$failed_list $item"
        fi
    done

    sync

    # output failed files...
    if [ ! -z "${failed_list}" ]; then
        echo "Failed to download some files:$failed_list"
    fi

}

# Load autojump profile.
if [ -n "${BASH}" ]; then
    shell="bash"
elif [ -n "${ZSH_NAME}" ]; then
    shell="zsh"
elif [ -n "${__fish_datadir}" ]; then
    shell="fish"
elif [ -n "${version}" ]; then
    shell="tcsh"
else
    shell=$(echo ${SHELL} | awk -F/ '{ print $NF }')
fi

function load_shell_extentions ()
{
    local dir=$1
    local pattern=$2

    if [ -d ${dir} ]; then
        for sh in `find ${dir} -maxdepth 1 -name "${pattern}"`; do
	        [ -r "$sh" ] && source $sh
        done
    fi
}

# prevent circular loop for sh shells
if [ "${shell}" = "sh" ]; then
    echo "skip" > /dev/null
else
    # special handling for autojump
    load_shell_extentions ~/.autojump/share/autojump/ "*.${shell}"
    load_shell_extentions /usr/share/autojump/ "*.${shell}"
fi


load_shell_extentions /usr/local/etc/profile.d/ "*.sh"
load_shell_extentions /usr/local/etc/profile.d/ "*.${shell}"
load_shell_extentions /etc/profile.d/ "*.sh"
load_shell_extentions /etc/profile.d/ "*.${shell}"
load_shell_extentions ${HOME} "*.${shell}"

alias ppid="ps -o ppid= -p"

function ppidof ()
{
    proc=$1
    pids=`pidof $proc`
    if [ -z "$pids" ]; then
        echo "Can't find process, make sure $1 is running..."
        return
    fi

    NF=`echo "$pids" | awk -F " " '{print NF}'`
    if [ $NF -gt 1 ]; then
        echo "Multiple process found for $proc, choose one:"
        echo "$pids"
        read pid
    else
        pid=$pids
    fi

    if [ -z $pid ]; then
        echo "Empty pid..."
        return
    fi

    ps -o ppid= -p $pid
}


function cp_usb ()
{
    PACK_DIR=
    case `uname -s` in
        Darwin)
            PACK_DIR=`mount | grep "/Volumes/" | awk -F " " '{print $3}'`
            ;;
        *)
            PACK_DIR=`mount | grep "/run/media" | awk -F " " '{print $3}'`
            ;;
    esac

    if [ -z ${PACK_DIR} ]; then
        echo "No usb disk detected"
        return 1
    fi

    cp $* ${PACK_DIR}

    printf "\nFlushing data...\n"
    sync
    echo "\nDone\n\n"
}

# Similar to update_env_from, but export vars immediately.
function update_env_from ()
{
    local target=`realpath $1`

    # skip if directory not exists...
    [ -d $target ] || return 0

    [ -d "$target/usr" ] && update_env_from $target/usr

    local exported=()

    local vars=()
    [ -d "$target/bin" ] && vars+=("$target/bin")
    [ -d "$target/sbin" ] && vars+=("$target/sbin")

    if [ ${#vars[@]} -ne 0 ]; then
        for p in $(echo ${vars[@]}); do
            PATH="$p:$PATH"
        done

        export PATH
        exported+=("PATH")
    fi

    vars=()
    [ -d "$target/lib" ] && vars+=("$target/lib")
    [ -d "$target/lib64" ] && vars+=("$target/lib64")

    if [ ${#vars[@]} -ne 0 ]; then
        for p in $(echo ${vars[@]}); do
            LD_LIBRARY_PATH="$p:$LD_LIBRARY_PATH"
        done
        export LD_LIBRARY_PATH
        exported+=("LD_LIBRARY_PATH")
    fi

    if [ -d "$target/lib/pkgconfig" ]; then
        export PKG_CONFIG_PATH="$target/lib/pkgconfig:${PKG_CONFIG_PATH}"
        exported+=("PKG_CONFIG_PATH")
    fi


    if [ ${#exported[@]} -ne 0 ]; then
        echo "Updated envs: ${exported[*]}..."
    else
        echo "Nothing was found in $target ..."
    fi
}

alias uef="update_env_from"
