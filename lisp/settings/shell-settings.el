(setq sh-getopt-template
      "while getopts \"ha:\" OPT; do
    case ${OPT} in
    h)
        less \"$0\"
        exit 0
        ;;
    *)
        echo \"unrecognized flag: ${OPT}\" && exit ${LINENO}
        ;;
    \?)
        echo \"Error: Invalid option or missing argument.\" >&2
        exit 1
        ;;
    esac
done
shift $((OPTIND -1))
")
