#!/usr/bin/env bash

NC='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'

msg_info() {
    echo -e "${GREEN}$1${NC}"
}

msg_warn() {
    echo -e "${YELLOW}$1${NC}"
}

msg_error() {
    echo -e "${RED}$1${NC}"
}

print_help() {
    echo
    echo "testRSA - a quick script to run tests all night long"
    echo
    echo "USAGE:"
    echo "    testRSA OPTION [...]"
    echo
    echo "OPTIONs are:"
    echo "    -o | --ontology <path>:"
    echo "        path to ontology."
    echo "    -d | --data <path>:"
    echo "        path to a folder containing data for the ontology."
    echo "    -q | --queries <path>:"
    echo "        path to a folder containing SPARQL query files to be"
    echo "        executed against the ontology and data."
    echo "    -j | --jar <path>:"
    echo "        path to the fat jar to be executed."
    echo "    -p | --prefix <path>:"
    echo "        provides a folder to prefix to the output files."
    echo "        Defaults to './results'."
    echo "    -h | -? | --help:"
    echo "        print this help"
    echo
}

ONTOLOGY=""
DATA=""
QUERIES=""
JAR=""
PREFIX="./results"

while [[ $# -gt 0 ]]
do
    case $1 in
        -o|--ontology)
            shift
            ONTOLOGY="$1"
            [ ! -r "$ONTOLOGY" ] && \
                msg_error "Unable to read '$ONTOLOGY'" && \
                print_help && \
                exit 2
            ;;
        -d|--data)
            shift
            DATA="$1"
            [ ! -d "$DATA" ] && \
                msg_error "'$DATA' is not a directory" && \
                print_help && \
                exit 2
            ;;
        -q|--queries)
            shift
            QUERIES="$1"
            [ ! -d "$QUERIES" ] && \
                msg_error "'$QUERIES' is not a directory" && \
                print_help && \
                exit 2
            ;;
        -j|--jar)
            shift
            JAR="$1"
            [ ! -r "$JAR" ] && \
                msg_error "Unable to read jar '$JAR'" && \
                print_help && \
                exit 2
            ;;
        -p|--prefix)
            shift
            PREFIX="$1"
            ;;
        -h|-?|--help)
            print_help
            exit 0
            ;;
        *)
            msg_error "$OPTION: invalid option"
            print_help
            exit 1
            ;;
    esac
    shift
done

[ -z "$ONTOLOGY" ] && \
    msg_error "Use -o | --ontology to provide an ontology file" && \
    print_help && \
    exit 3

[ -z "$DATA" ] && \
    msg_error "Use -d | --data to provide a data folder" && \
    print_help && \
    exit 3

[ -z "$QUERIES" ] && \
    msg_error "Use -q | --queries to provide a query folder" && \
    print_help && \
    exit 3

[ -z "$JAR" ] && \
    msg_error "Use -j | --jar to provide a jar file" && \
    print_help && \
    exit 3

DATAS=`\ls $DATA/*`
mkdir -p "$PREFIX"
for QUERY in "$QUERIES"/query*.sparql
do
    #sbt "run $QUERY $ONTOLOGY $DATAS" 2>&1 | tee "$PREFIX/answers_$(basename $QUERY .sparql).txt"
    java -cp ./lib/JRDFox.jar:"$JAR" uk.ac.ox.cs.rsacomb.RSAComb -q "$QUERY" "$ONTOLOGY" "$DATA"/* 2>&1 | tee "$PREFIX/answers_$(basename $QUERY .sparql).txt"
done

OUTPUT="$PREFIX/results.csv"
echo "NAME, TBOX, RBOX, ABOX, \
    CANONICAL MODEL GENERATION, \
    CANONICAL MODEL RULES, CANONICAL MODEL RULES LOADING, \
    CANONICAL MODEL FACTS, CANONICAL MODEL FACTS LOADING, \
    CANONICAL MODEL IDB, CANONICAL MODEL EDB, \
    FILTERING PROGRAM GENERATION, \
    FILTERING PROGRAM RULES, FILTERING PROGRAM RULES LOADING, \
    FILTERING PROGRAM FACTS, FILTERING PROGRAM FACTS LOADING, \
    FILTERING PROGRAM IDB, FILTERING PROGRAM EDB, \
    ANSWERING TIME, #ANSWERS, #UNFILTERED, #SPURIOUS, %SPURIOUS" > "$OUTPUT"

for RESULT in "$PREFIX"/*.txt
do
    awk -v filename="$RESULT" '
        BEGIN { 
            OFS = ", "
            name = filename
            sub("^.*answers_", "", name)
            sub(".txt$", "", name)
        }
        /Original TBox/ { tbox_size = $NF }
        /Original RBox/ { rbox_size = $NF }
        /Original ABox/ { abox_size = $NF }
        /Generating canonical model program \(END\)/ { canon_gen_time = $NF }
        /Generating filtering program \(END\)/ { filter_gen_time = $NF }
        /Canonical model rules/ {
            canon_rules = $NF
            canon = 1
        }
        /Canonical model facts/ {
            canon_facts = $NF
            canon = 1
        }
        /Filtering program rules/ {
            filter_rules = $NF
        }
        /Filtering program facts/ {
            filter_facts = $NF
        }
        /Loading rules \(END\)/ {
            if (canon) {
                canon_rules_load = $NF
            } else {
                filter_rules_load = $NF
            }
        }
        /Loading facts/ {
            if (canon) {
                canon_facts_load = $NF
            } else {
                filter_facts_load = $NF
            }
        }
        /Aggregate number of IDB facts/ {
            sub("^.*=", "")
            sub(",$", "")
            if (canon) {
                canon_idb = $0
            } else {
                filter_idb = $0
            }
        }
        /Aggregate number of EDB facts/ {
            sub("^.*=", "")
            sub(",$", "")
            if (canon) {
                canon_edb = $0
                canon = 0
            } else {
                filter_edb = $0
            }
        }
        /Answers computation \(END\)/ { answers_time = $NF }
        /Number of answers/ { answers = $(NF-1) }
        /Number of unfiltered answers/ { unfiltered = $(NF-1) }
        /Number of spurious answers/ { spurious = $(NF-1) }
        /Percentage of spurious answers/ { spurious_perc = $NF }
        END { print name, tbox_size, rbox_size, abox_size, \
                    canon_gen_time, canon_rules, canon_rules_load, canon_facts, canon_facts_load, \
                    canon_idb, canon_edb, \
                    filter_gen_time, filter_rules, filter_rules_load, filter_facts, filter_facts_load, \
                    filter_idb, filter_edb, \
                    answers_time, answers, unfiltered, spurious, spurious_perc
        }
    ' "$RESULT" >> "$OUTPUT"
done

exit 0
