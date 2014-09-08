#! /bin/sh

dir=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
mycat=$dir/../../bin/cat

testSingleFile()
{
    expected=$(cat $dir/a.example)
    actual=$($mycat $dir/a.example)
    assertEquals "${expected}" "${actual}"
}

. shunit2
