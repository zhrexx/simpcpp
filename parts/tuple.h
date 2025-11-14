#include <cstddef>
#include <utility>
#include <type_traits>

template<typename... Ts>
class Tuple;

template<>
class Tuple<> {
public:
    constexpr Tuple() = default;
};

template<typename T, typename... Rest>
class Tuple<T, Rest...> : private Tuple<Rest...> {
    T value;
    using Base = Tuple<Rest...>;

public:
    constexpr Tuple() = default;

    constexpr explicit Tuple(const T& v, const Rest&... rest)
        : Base(rest...), value(v) {}

    constexpr explicit Tuple(T&& v, Rest&&... rest)
        : Base(std::forward<Rest>(rest)...), value(std::forward<T>(v)) {}

    template<std::size_t I>
    constexpr void set(auto&& val) & {
        if constexpr (I == 0) {
            value = std::forward<decltype(val)>(val);
        } else {
            Base::template set<I - 1>(std::forward<decltype(val)>(val));
        }
    }

    template<typename F>
    constexpr void map_mut(F&& f) & {
        map_mut_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr void apply_mut(F&& f) & {
        apply_mut_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<std::size_t N>
    constexpr void fill(const auto& val) & {
        fill_impl<N>(val, std::make_index_sequence<N>{});
    }

    template<typename U>
    constexpr void push_front_mut(U&& val) & {
        *this = push_front(std::forward<U>(val));
    }

    template<typename U>
    constexpr void push_back_mut(U&& val) & {
        *this = push_back(std::forward<U>(val));
    }

    constexpr void pop_front_mut() & {
        *this = pop_front();
    }

    constexpr void reverse_mut() & {
        *this = reverse();
    }

    template<typename Pred>
    constexpr void filter_mut(Pred&& pred) & {
        *this = filter(std::forward<Pred>(pred));
    }

    constexpr void swap(Tuple& other) & {
        swap_impl(other, std::index_sequence_for<T, Rest...>{});
    }

    template<std::size_t I, std::size_t J>
    constexpr void swap_elements() & {
        auto temp = std::move(get<I>());
        get<I>() = std::move(get<J>());
        get<J>() = std::move(temp);
    }

    template<std::size_t I>
    constexpr const auto& get() const& {
        if constexpr (I == 0) {
            return value;
        } else {
            return Base::template get<I - 1>();
        }
    }

    template<std::size_t I>
    constexpr auto&& get() && {
        if constexpr (I == 0) {
            return std::move(value);
        } else {
            return std::move(*this).Base::template get<I - 1>();
        }
    }

    template<typename F>
    constexpr auto apply(F&& f) & {
        return apply_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr auto apply(F&& f) const& {
        return apply_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr auto apply(F&& f) && {
        return std::move(*this).apply_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr auto map(F&& f) & {
        return map_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr auto map(F&& f) const& {
        return map_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr auto map(F&& f) && {
        return std::move(*this).map_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr void for_each(F&& f) & {
        for_each_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr void for_each(F&& f) const& {
        for_each_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    static constexpr std::size_t size() {
        return sizeof...(Rest) + 1;
    }

    template<typename U>
    constexpr auto push_front(U&& val) const& {
        return push_front_impl(std::forward<U>(val), std::index_sequence_for<T, Rest...>{});
    }

    template<typename U>
    constexpr auto push_front(U&& val) && {
        return std::move(*this).push_front_impl(std::forward<U>(val), std::index_sequence_for<T, Rest...>{});
    }

    template<typename U>
    constexpr auto push_back(U&& val) const& {
        return push_back_impl(std::forward<U>(val), std::index_sequence_for<T, Rest...>{});
    }

    template<typename U>
    constexpr auto push_back(U&& val) && {
        return std::move(*this).push_back_impl(std::forward<U>(val), std::index_sequence_for<T, Rest...>{});
    }

    constexpr auto pop_front() const& {
        return static_cast<const Base&>(*this);
    }

    constexpr auto pop_front() && {
        return static_cast<Base&&>(*this);
    }

    constexpr auto& front() & { return get<0>(); }
    constexpr const auto& front() const& { return get<0>(); }
    constexpr auto&& front() && { return std::move(*this).template get<0>(); }

    constexpr auto& back() & { return get<sizeof...(Rest)>(); }
    constexpr const auto& back() const& { return get<sizeof...(Rest)>(); }
    constexpr auto&& back() && { return std::move(*this).template get<sizeof...(Rest)>(); }

    template<std::size_t N>
    constexpr auto take() const& {
        return take_impl<N>(std::make_index_sequence<N>{});
    }

    template<std::size_t N>
    constexpr auto take() && {
        return std::move(*this).template take_impl<N>(std::make_index_sequence<N>{});
    }

    template<std::size_t N>
    constexpr auto drop() const& {
        return drop_impl<N>(std::make_index_sequence<size() - N>{});
    }

    template<std::size_t N>
    constexpr auto drop() && {
        return std::move(*this).template drop_impl<N>(std::make_index_sequence<size() - N>{});
    }

    template<typename... Us>
    constexpr auto concat(const Tuple<Us...>& other) const& {
        return concat_impl(other, std::index_sequence_for<T, Rest...>{}, std::index_sequence_for<Us...>{});
    }

    template<typename... Us>
    constexpr auto concat(Tuple<Us...>&& other) && {
        return std::move(*this).concat_impl(std::move(other), std::index_sequence_for<T, Rest...>{}, std::index_sequence_for<Us...>{});
    }

    constexpr auto reverse() const& {
        return reverse_impl(std::index_sequence_for<T, Rest...>{});
    }

    constexpr auto reverse() && {
        return std::move(*this).reverse_impl(std::index_sequence_for<T, Rest...>{});
    }

    template<typename F>
    constexpr auto fold_left(F&& f) const& {
        return fold_left_impl(std::forward<F>(f), std::index_sequence_for<T, Rest...>{});
    }

    template<typename F, typename Init>
    constexpr auto fold_left(F&& f, Init&& init) const& {
        return fold_left_init_impl(std::forward<F>(f), std::forward<Init>(init), std::index_sequence_for<T, Rest...>{});
    }

    template<typename Pred>
    constexpr auto filter(Pred&& pred) const& {
        return filter_impl(std::forward<Pred>(pred), std::index_sequence_for<T, Rest...>{});
    }

    template<typename U>
    constexpr bool contains(const U& val) const& {
        return contains_impl(val, std::index_sequence_for<T, Rest...>{});
    }

private:
    template<typename F, std::size_t... Is>
    constexpr auto apply_impl(F&& f, std::index_sequence<Is...>) & {
        return std::forward<F>(f)(get<Is>()...);
    }

    template<typename F, std::size_t... Is>
    constexpr auto apply_impl(F&& f, std::index_sequence<Is...>) const& {
        return std::forward<F>(f)(get<Is>()...);
    }

    template<typename F, std::size_t... Is>
    constexpr auto apply_impl(F&& f, std::index_sequence<Is...>) && {
        return std::forward<F>(f)(std::move(*this).template get<Is>()...);
    }

    template<typename F, std::size_t... Is>
    constexpr auto map_impl(F&& f, std::index_sequence<Is...>) & {
        return Tuple{std::forward<F>(f)(get<Is>())...};
    }

    template<typename F, std::size_t... Is>
    constexpr auto map_impl(F&& f, std::index_sequence<Is...>) const& {
        return Tuple{std::forward<F>(f)(get<Is>())...};
    }

    template<typename F, std::size_t... Is>
    constexpr auto map_impl(F&& f, std::index_sequence<Is...>) && {
        return Tuple{std::forward<F>(f)(std::move(*this).template get<Is>())...};
    }

    template<typename F, std::size_t... Is>
    constexpr void for_each_impl(F&& f, std::index_sequence<Is...>) & {
        (std::forward<F>(f)(get<Is>()), ...);
    }

    template<typename F, std::size_t... Is>
    constexpr void for_each_impl(F&& f, std::index_sequence<Is...>) const& {
        (std::forward<F>(f)(get<Is>()), ...);
    }

    template<typename U, std::size_t... Is>
    constexpr auto push_front_impl(U&& val, std::index_sequence<Is...>) const& {
        return Tuple<std::decay_t<U>, T, Rest...>{std::forward<U>(val), get<Is>()...};
    }

    template<typename U, std::size_t... Is>
    constexpr auto push_front_impl(U&& val, std::index_sequence<Is...>) && {
        return Tuple<std::decay_t<U>, T, Rest...>{std::forward<U>(val), std::move(*this).template get<Is>()...};
    }

    template<typename U, std::size_t... Is>
    constexpr auto push_back_impl(U&& val, std::index_sequence<Is...>) const& {
        return Tuple<T, Rest..., std::decay_t<U>>{get<Is>()..., std::forward<U>(val)};
    }

    template<typename U, std::size_t... Is>
    constexpr auto push_back_impl(U&& val, std::index_sequence<Is...>) && {
        return Tuple<T, Rest..., std::decay_t<U>>{std::move(*this).template get<Is>()..., std::forward<U>(val)};
    }

    template<std::size_t N, std::size_t... Is>
    constexpr auto take_impl(std::index_sequence<Is...>) const& {
        return Tuple<std::tuple_element_t<Is, Tuple>...>{get<Is>()...};
    }

    template<std::size_t N, std::size_t... Is>
    constexpr auto take_impl(std::index_sequence<Is...>) && {
        return Tuple<std::tuple_element_t<Is, Tuple>...>{std::move(*this).template get<Is>()...};
    }

    template<std::size_t N, std::size_t... Is>
    constexpr auto drop_impl(std::index_sequence<Is...>) const& {
        return Tuple<std::tuple_element_t<N + Is, Tuple>...>{get<N + Is>()...};
    }

    template<std::size_t N, std::size_t... Is>
    constexpr auto drop_impl(std::index_sequence<Is...>) && {
        return Tuple<std::tuple_element_t<N + Is, Tuple>...>{std::move(*this).template get<N + Is>()...};
    }

    template<typename... Us, std::size_t... Is, std::size_t... Js>
    constexpr auto concat_impl(const Tuple<Us...>& other, std::index_sequence<Is...>, std::index_sequence<Js...>) const& {
        return Tuple<T, Rest..., Us...>{get<Is>()..., other.template get<Js>()...};
    }

    template<typename... Us, std::size_t... Is, std::size_t... Js>
    constexpr auto concat_impl(Tuple<Us...>&& other, std::index_sequence<Is...>, std::index_sequence<Js...>) && {
        return Tuple<T, Rest..., Us...>{std::move(*this).template get<Is>()..., std::move(other).template get<Js>()...};
    }

    template<std::size_t... Is>
    constexpr auto reverse_impl(std::index_sequence<Is...>) const& {
        return Tuple<std::tuple_element_t<sizeof...(Is) - 1 - Is, Tuple>...>{get<sizeof...(Is) - 1 - Is>()...};
    }

    template<std::size_t... Is>
    constexpr auto reverse_impl(std::index_sequence<Is...>) && {
        return Tuple<std::tuple_element_t<sizeof...(Is) - 1 - Is, Tuple>...>{std::move(*this).template get<sizeof...(Is) - 1 - Is>()...};
    }

    template<typename F, std::size_t... Is>
    constexpr auto fold_left_impl(F&& f, std::index_sequence<Is...>) const& {
        return fold_left_helper(std::forward<F>(f), get<Is>()...);
    }

    template<typename F, typename Init, std::size_t... Is>
    constexpr auto fold_left_init_impl(F&& f, Init&& init, std::index_sequence<Is...>) const& {
        return fold_left_helper(std::forward<F>(f), std::forward<Init>(init), get<Is>()...);
    }

    template<typename F, typename Acc, typename U, typename... Us>
    static constexpr auto fold_left_helper(F&& f, Acc&& acc, U&& u, Us&&... us) {
        if constexpr (sizeof...(Us) == 0) {
            return std::forward<F>(f)(std::forward<Acc>(acc), std::forward<U>(u));
        } else {
            return fold_left_helper(std::forward<F>(f), std::forward<F>(f)(std::forward<Acc>(acc), std::forward<U>(u)), std::forward<Us>(us)...);
        }
    }

    template<typename Pred, std::size_t... Is>
    constexpr auto filter_impl(Pred&& pred, std::index_sequence<Is...>) const& {
        return filter_helper(std::forward<Pred>(pred), get<Is>()...);
    }

    template<typename Pred, typename... Us>
    static constexpr auto filter_helper(Pred&&, std::bool_constant<true>) {
        return Tuple<>{};
    }

    template<typename Pred, typename U, typename... Us>
    static constexpr auto filter_helper(Pred&& pred, std::bool_constant<true>, U&& u, Us&&... us) {
        if constexpr (sizeof...(Us) == 0) {
            if (pred(u)) {
                return Tuple<std::decay_t<U>>{std::forward<U>(u)};
            } else {
                return Tuple<>{};
            }
        } else {
            if (pred(u)) {
                return Tuple<std::decay_t<U>>{std::forward<U>(u)}.concat(filter_helper(std::forward<Pred>(pred), std::bool_constant<true>{}, std::forward<Us>(us)...));
            } else {
                return filter_helper(std::forward<Pred>(pred), std::bool_constant<true>{}, std::forward<Us>(us)...);
            }
        }
    }

    template<typename U, std::size_t... Is>
    constexpr bool contains_impl(const U& val, std::index_sequence<Is...>) const& {
        return ((get<Is>() == val) || ...);
    }
};

namespace detail {
    template<std::size_t I, typename T, typename... Rest>
    struct TupleElement {
        using type = typename TupleElement<I - 1, Rest...>::type;
    };

    template<typename T, typename... Rest>
    struct TupleElement<0, T, Rest...> {
        using type = T;
    };
}

template<typename... Ts>
constexpr auto make_tuple(Ts&&... args) {
    return Tuple<std::decay_t<Ts>...>{std::forward<Ts>(args)...};
}

template<std::size_t I, typename... Ts>
constexpr auto& get(Tuple<Ts...>& t) {
    return t.template get<I>();
}

template<std::size_t I, typename... Ts>
constexpr const auto& get(const Tuple<Ts...>& t) {
    return t.template get<I>();
}

template<std::size_t I, typename... Ts>
constexpr auto&& get(Tuple<Ts...>&& t) {
    return std::move(t).template get<I>();
}
