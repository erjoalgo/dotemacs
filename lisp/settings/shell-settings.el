(setq sh-getopt-template
"while getopts \"ha:\" OPT; do
    case ${OPT} in
    a)
        SOMEARG=${OPTARG}
        ;;
    h)
        less $0
        exit 0
        ;;
    esac
done
shift $((OPTIND -1))
")
