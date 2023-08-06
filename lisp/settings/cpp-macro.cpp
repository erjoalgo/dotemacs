template<typename T>
void print_vector(std::vector<T> v) {
    for (const auto& elm : v) {
        cout << elm << ' ';
    }
}

template<typename T>
void print_vector(vector<vector<T>> v) {
    for (const auto& elm : v) {
        print_vector(elm);
        cout << '\n';
    }
}
