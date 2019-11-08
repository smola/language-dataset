module sorty

fn get_max(nums []int, n int) int {
	mut mx := nums[0]
	for i := 1; i < n; i++ {
		if nums[i] > mx {
			mx = nums[i]
		}
	}
	return mx
}

fn count(nums mut []int, n, exp int) {
	mut output := [0].repeat(n)
	mut count := [0].repeat(10)

	for i := 0; i < n; i++ {
		count[ (nums[i] / exp) % 10 ]++
	}

	for i := 1; i < 10; i++ {
		count[i] += count[i - 1]
	}

	for i := n -1; i >= 0; i-- {
		output[count[ (nums[i] / exp) %10 ] - 1] = nums[i]
		count[ (nums[i] / exp) % 10 ]--
	}

	for i := 0; i < n; i++ {
		nums[i] = output[i]
	}
}

pub fn radix(nums mut []int, n int) {
	m := get_max(nums, n)

	for exp := 1; m / exp > 0; exp *= 10 {
		count(mut nums, n, exp)
	}
}