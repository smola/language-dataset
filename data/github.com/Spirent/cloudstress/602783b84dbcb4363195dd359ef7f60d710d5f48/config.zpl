# Copyright 2017 Spirent Communications, All Rights Reserverd
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on a "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either expressed or implied

#rfc.zeromq.org/spec:4/ZPL

generator
    cpu
        utilization = 100.0
        running = "true"
    block
        writes_per_sec = 8000
        write_size = 2M
        reads_per_sec = 10000
        read_size = 2M
        queue_depth = 4
        vdev_path = "/tmp"
        vdev_size = 10000000
        file_size = 1000
        running = "true"
        pattern = "random"
    memory
        writes_per_sec = 8000
        write_size = 2000000
        reads_per_sec = 10000
        read_size = 2000000
        buffer_size = 250000000
        running = "true"
        pattern = "random"
