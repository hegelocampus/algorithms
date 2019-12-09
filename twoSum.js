// for unsorted input
let twoSum (nums, target) => {
  let pairs = {};

  for (let i = 0; i < nums.length; i++) {
    let p = pairs[nums[i]];
    if (p !== undefined) {
      return [p, i];
    } else {
      pairs[target - nums[i]] = i;
    }
  }
}

