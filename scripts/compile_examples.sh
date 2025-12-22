#!/bin/sh

set -xe

for i in examples/*; do
	pushd $i
	../../target/debug/heron main.he
	popd
done

