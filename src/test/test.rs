use cargo_snippet::snippet;

// こんなかんじでアノテーションで以下の関数がスニペットであることを指定します
// この場合mymathとgcdという名前のスニペットであることを表しています
# [snippet("mymath")]
# [snippet("gcd")]
fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

// こっちの書き方でもいいです
# [snippet(name = "mymath")]
// 名前を省略すると関数名がそのままスニペット名になります
# [snippet]
fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

# [snippet]
// スニペットに依存関係がある場合は以下のように指定できます
# [snippet(include = "gcd")]
fn gcd_list(list: &[u64]) -> u64 {
    list.iter().fold(list[0], |a, b| gcd(a, b));
}

# [test]
fn test_gcd() {
    assert_eq!(gcd(57, 3), 3);
}

# [test]
fn test_lcm() {
    assert_eq!(lcm(3, 19), 57);
}

