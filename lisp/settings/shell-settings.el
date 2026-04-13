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
    esac
done
shift $((OPTIND -1))
")
