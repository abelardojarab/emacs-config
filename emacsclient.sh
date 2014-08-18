#!/bin/sh
nowait=0
if [ "$1" = "-n" ]; then
    nowait=1
    shift
fi

if [ "$#" -eq 0 ]; then
    echo "no files specified"
    exit 0
fi

[ -e "$remote_emacs_auth" ] && client_host=$(sed -n 1p "$remote_emacs_auth")
if [ -z "$client_host" ] || ! nc -z $client_host; then
    echo "no emacs server"
    sleep 1
    exec emacs "$@"
fi

quote () {
    sed 's:&:\&\&:g;s:-:\&-:g;s: :\&_:g'
}

quoteline () {
    printf "%s\n" "$1" | quote
}

unquote () {
    sed 's:&&:\&:g;s:&-:-:g;s:&_: :g;s:&n:\
:g'
}

client_auth=$(sed -n 2p "${remote_emacs_auth}")
tramp_prefix=$(sed -n 3p "${remote_emacs_auth}")
args=$(printf "%s\n%s\n" "${client_auth}" "-dir ${tramp_prefix}${quoted_pwd}")
[ "${nowait}" != 0 ] && args="${args} -nowait"
quoted_pwd=$(quoteline "$(pwd)")

for file; do
    quoted_file=$(quoteline "${file}")
    case "${file}" in
        +*) argument="-position ${quoted_file}";;
        /*) argument="-file ${tramp_prefix}${quoted_file}";;
        *)  argument="-file ${tramp_prefix}${quoted_pwd}/${quoted_file}";;
    esac
    args="${args} ${argument}"
done

printf "%s\n" "${args}" | nc $client_host | unquote
