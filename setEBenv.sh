SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PATH=$SCRIPT_DIR/bin:$PATH
export JBCOBJECTLIST=$SCRIPT_DIR/lib:$JBCOBJECTLIST
