package error

import "fmt"

var (
	SCAN_ERROR    bool = false
	PARSE_ERROR   bool = false
	RUNTIME_ERROR bool = false
)

func ScanPanic(path *string, line int, reason string) {
	SCAN_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s\n", *path, line, reason)
	panic(msg)
}

func ParsePanic(path *string, line int, reason string) {
	msg := fmt.Sprintf("%s:%d: %s", *path, line, reason)
	panic(msg)
}

func ParsePanicRecover(reason string) {
	PARSE_ERROR = true
	fmt.Println(reason)
}

func RuntimePanic(path *string, reason string, expr ...interface{}) {
	msg := fmt.Sprintf("%s: %v: %s", *path, expr, reason)
	panic(msg)
}

func RuntimePanicRecover(reason string) {
	RUNTIME_ERROR = true
	fmt.Println(reason)
}