# C++ Utility: `Monad`

## Overview

 This is `Monad`, a C++ module that provides monadic interface for C++.

 *`Monad`* is *a concept in functional programming*. It is a design pattern that allows structuring programs generically while automating away boilerplate code needed by the program logic. ==*Monads* achieve this by providing their own data type, which represents a specific form of computation, along with one procedure to wrap values of any basic type within the monad (yielding a monadic value) and another to compose functions that output monadic values (called monadic functions)==.  
<sub><sup>from [Wikipedia](https://en.wikipedia.org/wiki/Monad_(functional_programming))</sup></sub>

 `Monad` can forward its internal state to the next function flawlessly. It is very useful in functional programming. Because they are pure functions so are capable of stateless, pure functions, no side effects.

 In C++, `Method Chaining` simulates the pure functional programming, Hiding its value and state, and only providings the interface to access the **Perfect value**. `noexcept` s*t*imulates no side effects. Unfortunately there is no dictionary meaning pure function with no side effect in C++ <sub><sup>(unless they are metafunctions)</sup></sub>.

### Installation

Just a header file. Copy it to your project.

### Requirements

- C++ Modules Support (`C++20` or later)  
	<sub><sup>if you are using `#include`</sup></sub>

- C++ Standard Library Modules Support (`C++23` or later)  
	<sub><sup>if you are using `import;`</sup></sub>

## API

- `Monad` has `[[nodiscard]]` attribute.

- The underlying type `T` should be **copyable**;

##### import

~~~C++
import Utility.Monad;
~~~

Imports `util::Monad`.

---

##### `Monad()`

~~~C++
// Default initializer
util::Monad<T> monad{};

// Constexpr initializer
constexpr util::Monad<T> cmonad{};
~~~

---

##### `Monad(monad)`

~~~C++
// Copy constructors
util::Monad<T> pmonad1{ monad };
constexpr util::Monad<T> pmonad2{ cmonad };

// Move constructors
util::Monad<T> mmonad1{ std::move(monad) };
constexpr util::Monad<T> mmonad2{ std::move(cmonad) };
~~~

---

##### `if_then(invocable<Type, Args...>, Args...) -> self` [^1]

~~~C++
Monad& if_then(functor, [args...]) noexcept(?)
~~~

 Executes the `functor` if this `Monad` has a value, and returns the `Monad` itself.

---

##### `and_then(invocable<Type, Args...>, Args...) -> Monad` [^1]

~~~C++
Monad<U> and_then<U>(functor: Monad<U>, [args...]) noexcept(?)
~~~

 If this `Monad` has a value, executes `functor` and returns it, or returns empty `Monad<U>`.

 `functor` should have to return a specialization of `Monad` (aka. `Monad<U>`).

---

##### `transform<R>(invocable_r<R, Type>, invocable_r<R, Type>) -> R` [^1][^2]

~~~C++
R and_then<R>(safe: R, fail: R) noexcept(?)
~~~

 If this `Monad` has a value, executes `safe` and returns it, or executes `fail` and returns it.

 Both invocables should have to return the same type.

---

##### `forward<U>(invocable<Type>, U&& default_value) -> U` [^1]

~~~C++
U forward<U>(functor: U, default_value: U) noexcept(?)
~~~

 If this `Monad` has a value, executes `functor` and returns it, or returns empty `default_value`.

---

##### `else_than(invocable<Type, Args...>, Args...) -> self` [^1]

~~~C++
Monad& else_than<U>(functor, [args...]) noexcept(?)
~~~

 Executes the `functor` if this `Monad` has ==no value==, and returns the `Monad` itself.

---

##### `or_else(invocable<Type, Args...>, Args...) -> Monad` [^1]

~~~C++
Monad<U> or_else<U>(functor: Monad<U>, [args...]) noexcept(?)
~~~

 If this `Monad` has ==no value==, executes `functor` and returns it, or returns empty `Monad<U>`.

 `functor` should have to return a specialization of `Monad` (aka. `Monad<U>`).

---

##### `has_value() -> bool`

~~~C++
bool has_value() const noexcept
~~~

---

##### `value() & -> T&` [^3]

~~~C++
T& value() & noexcept
~~~

---

##### `value() const& -> const T&` [^3]

~~~C++
const T& value() const& noexcept
~~~

---

##### `value() && -> T&&` [^3]

~~~C++
T&& value() && noexcept
~~~

---

##### `value() const&& -> const T&&` [^3]

~~~C++
const T&& value() const&& noexcept
~~~

---

##### `value_or(const T&) const& -> T` [^4] [^6]

~~~C++
T value_or(const T&) const& noexcept
~~~

---

##### `value_or(T&&) const& -> T` [^4] [^6]

~~~C++
T value_or(T&&) const& noexcept
~~~

---

##### `value_or(const T&) && -> T` [^4] [^6]

~~~C++
T value_or(const T&) && noexcept
~~~

---

##### `value_or(T&&) && -> T&&` [^5] [^7]

~~~C++
T&& value_or(T&&) && noexcept
~~~

---

##### `get() const& -> T` [^4] [^6]

~~~C++
T get() const& noexcept
~~~

---

##### `get() && -> T&&` [^4] [^7]

~~~C++
T&& get() && noexcept
~~~

---

##### `explicit operator T() const` [^4] [^6]

~~~C++
explicit operator T() const noexcept
~~~

---

##### `explicit operator bool() const`

~~~C++
explicit operator bool() const noexcept
~~~

---

## Usage

~~~C++
import Utility.Monad;

// They always have a state.
Monad<int> Send(::SOCKET handle, const char* buffer, const int& size)
{
	return ::send(handle, buffer, size, 0);
}

// They always have a state.
Monad<int> Recv(::SOCKET handle, char* buffer, const int& size)
{
	return ::recv(handle, buffer, size, 0);
}

bool echo_once()
{
	::SOCKET my_socket;
	char my_recv_buffer[1024]{};
	int my_recv_size{};

	constexpr int FirstRecvSize = 1024;

	return Recv(my_socket, my_recv_buffer, FirstRecvSize).and_then(
		[&](int&& received_size) noexcept -> Monad<int>
		{
			if (0 < received_size)
			{
				return recv_size += received_size;
			}
			else 
			{
				return util::nullopt;
			}
		}
	).and_then([](int&& buffer_bytes) -> Monad<bool> {
		// packets
		// ...

		return true;
	}).value_or(false);
}
~~~

## Code Synopsis

~~~C++
template <std::copyable T>
class [[nodiscard]] Monad
{
public:
	template<typename Fn, typename... Args>
	using monad_result_t = std::remove_cvref_t<std::invoke_result_t<Fn, Args...>>;

	constexpr Monad() noexcept = default;
	constexpr ~Monad() noexcept = default;

	constexpr Monad(std::nullopt) noexcept;
	template<typename U>
	constexpr Monad(std::reference_wrapper<U>) noexcept = default;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&, Args...>
	constexpr Monad&
		if_then(Fn&& action, Args&&... args) &;
		
	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&, Args...>
	constexpr const Monad&
		if_then(Fn&& action, Args&&... args) const&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&&, Args...>
	constexpr Monad&&
		if_then(Fn&& action, Args&&... args) &&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&&, Args...>
	constexpr const Monad&&
		if_then(Fn&& action, Args&&... args) const&&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&, Args...>
	constexpr auto
		and_then(Fn&& action, Args&&... args) &;
		
	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&, Args...>
	constexpr auto
		and_then(Fn&& action, Args&&... args) const&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&&, Args...>
	constexpr auto
		and_then(Fn&& action, Args&&... args) &&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&&, Args...>
	constexpr auto
		and_then(Fn&& action, Args&&... args) const&&;

	template<std::invocable<T&> Lfn, std::invocable<T&> Rfn>
	 constexpr
		monad_result_t<Lfn, const T&>
		transform(Lfn&& safe_action, Rfn&& fail_action) &;
		
	template<std::invocable<T&> Lfn, std::invocable<T&> Rfn>
	 constexpr
		monad_result_t<Lfn, const T&>
		transform(Lfn&& safe_action, Rfn&& fail_action) const&;

	template<std::invocable<T&&> Lfn, std::invocable<T&&> Rfn>
	 constexpr
		monad_result_t<Lfn, const T&&>
		transform(Lfn&& safe_action, Rfn&& fail_action) &&;

	template<std::invocable<const T&&> Lfn, std::invocable<const T&&> Rfn>
	 constexpr
		monad_result_t<Lfn, const T&&>
		transform(Lfn&& safe_action, Rfn&& fail_action) const&&;

	template<std::invocable<T&> Fn, typename Uty>
	constexpr
		Uty&&
		forward(Fn&& safe_action, Uty&& default_value) &;
		
	template<std::invocable<T&> Fn, typename Uty>
	constexpr
		Uty&&
		forward(Fn&& safe_action, Uty&& default_value) const&;

	template<std::invocable<T&> Fn, typename Uty>
	constexpr
		Uty&&
		forward(Fn&& safe_action, Uty&& default_value) &&;

	template<std::invocable<T&> Fn, typename Uty>
	constexpr
		Uty&&
		forward(Fn&& safe_action, Uty&& default_value) const&&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&, Args...>
	constexpr
		Monad&
		else_than(Fn&& action, Args&&... args) &;
		
	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&, Args...>
	constexpr
		const Monad&
		else_than(Fn&& action, Args&&... args) const&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&&, Args...>
	constexpr
		Monad&&
		else_than(Fn&& action, Args&&... args) &&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&&, Args...>
	constexpr
		const Monad&&
		else_than(Fn&& action, Args&&... args) &&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&, Args...>
	constexpr
		auto
		or_else(Fn&& action, Args&&... args) &;
		
	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&, Args...>
	constexpr
		auto
		or_else(Fn&& action, Args&&... args) const&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, T&&, Args...>
	constexpr
		auto
		or_else(Fn&& action, Args&&... args) &&;

	template<typename Fn, typename... Args>
		requires std::invocable<Fn, const T&&, Args...>
	constexpr
		auto
		or_else(Fn&& action, Args&&... args) &&;

	constexpr T& value() &;
	constexpr const T& value() const&;
	constexpr T& value() &&;
	constexpr const T& value() const&&;

	constexpr T get() const&;
	constexpr T&& get() &&;

	constexpr T value_or(const T& default_value) const&;
	constexpr T value_or(T&& default_value) const&;
	constexpr T value_or(const T& default_value) &&;
	constexpr T&& value_or(T&& default_value) &&;

	constexpr bool has_value() const;

	explicit constexpr operator T() const;
	explicit constexpr operator bool() const;

private:
	T _Value;
	bool _Has_value;
};
~~~

---

## Contribute

- Contributions are always welcome!

[^1]: `noexcept` if `invocable` is `noexcept`.

[^2]: `invocable_r<R, Type>` is a `invocable<Type>` that returns `R`.

[^3]: Perfect forwarding

[^4]: Copy construct

[^5]: Move construct

[^6]: `noexcept` if the copy construct is `noexcept`.

[^7]: `noexcept` if the move construct is `noexcept`.
