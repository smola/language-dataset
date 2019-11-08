// Copyright (c) 2019 Vitor Oliveira. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import flag
import os
import rand
import time

fn main() {
    mut fp := flag.new_flag_parser(os.args)
    generator := fp.string('generator', '', 'faker generator name')
    method := fp.string('method', '', 'faker generator method name')
    path := './data/$generator/$method'

    if os.file_exists(path) {
        print_generator_sample(path)
    } else {
        println('File does not exist')
        return
    }
}

fn print_generator_sample(path string) {
    contents := os.read_file(path.trim_space()) or {
        println('Failed to open $path')
        return
    }

    lines := contents.split_into_lines()
    length := lines.len

    print_random_element(lines, length)
}

fn print_random_element(lines []string, length int) {
    rand.seed(time.now().uni)

    println(lines[rand.next(length-1)])
}
