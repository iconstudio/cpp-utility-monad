module;
#include <concepts>
#include <optional>

// Monadic interface module
export module Utility.Monad;

export namespace util
{
	template<typename Rx, typename Fn, typename... Args>
	concept invocable_results = std::invocable<Fn, Args...>&& std::convertible_to<std::invoke_result_t<Fn, Args...>, Rx>;

	template<typename Fn, typename... Args>
	using monad_result_t = std::remove_cvref_t<std::invoke_result_t<Fn, Args...>>;

	template <typename T, template <typename...> typename>
	inline constexpr bool is_specialization_v = false;

	template <template <typename...> typename T, typename... Args>
	inline constexpr bool is_specialization_v<T<Args...>, T> = true;

	template<typename T>
	class [[nodiscard]] Monad;

	template<std::copyable T>
	class [[nodiscard]] Monad<T>
	{
	public:
		using value_type = T;

		constexpr Monad(std::nullopt_t) noexcept(std::is_nothrow_constructible_v<T>)
			: myValue()
		{}

		template<std::convertible_to<T> Uty>
		constexpr Monad(Uty&& uty) noexcept(std::is_nothrow_constructible_v<T, Uty&&>)
			: myValue(std::forward<Uty>(uty))
			, hasValue(true)
		{}

		template<std::convertible_to<T> Uty>
		constexpr Monad(std::reference_wrapper<Uty> ref) noexcept(std::is_nothrow_copy_constructible_v<T>)
			: myValue(ref.get())
			, hasValue(true)
		{}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&, Args...>
		inline constexpr Monad&
			if_then(Fn&& action, Args&&... args) &
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&>(), std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&, Args...>
		inline constexpr const Monad&
			if_then(Fn&& action, Args&&... args) const&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&>(), std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&&, Args...>
		inline constexpr Monad&&
			if_then(Fn&& action, Args&&... args) &&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&&>(), std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}

			return std::move(*this);
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&&, Args...>
		inline constexpr const Monad&&
			if_then(Fn&& action, Args&&... args) const&&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&&>(), std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}

			return std::move(*this);
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&, Args...>
		inline constexpr
			auto
			and_then(Fn&& action, Args&&... args) &
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, T&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (hasValue)
			{
				return std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&, Args...>
		inline constexpr
			auto
			and_then(Fn&& action, Args&&... args) const&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, const T&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (hasValue)
			{
				return std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&&, Args...>
		inline constexpr
			auto
			and_then(Fn&& action, Args&&... args) &&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, T&&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (hasValue)
			{
				return std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&&, Args...>
		inline constexpr
			auto
			and_then(Fn&& action, Args&&... args) const&&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, const T&&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (hasValue)
			{
				return std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<std::invocable<T&> Lfn, std::invocable<T&> Rfn>
		inline constexpr
			monad_result_t<Lfn, T&>
			transform(Lfn&& safe_action, Rfn&& fail_action) &
			noexcept(noexcept(std::forward<Lfn>(safe_action)(std::declval<T&>())) && noexcept(std::forward<Rfn>(fail_action)(std::declval<T&>())))
		{
			using safe_result_t = monad_result_t<Lfn, T&>;
			using fail_result_t = monad_result_t<Rfn, T&>;

			static_assert(std::same_as<safe_result_t, fail_result_t> || (std::same_as<safe_result_t, void> && std::same_as<fail_result_t, void>));

			if (hasValue)
			{
				if constexpr (std::is_same_v<safe_result_t, void>)
				{
					std::forward<Lfn>(safe_action)(myValue);
				}
				else
				{
					return std::forward<Lfn>(safe_action)(myValue);
				}
			}
			else
			{
				if constexpr (std::is_same_v<fail_result_t, void>)
				{
					std::forward<Rfn>(fail_action)(myValue);
				}
				else
				{
					return std::forward<Rfn>(fail_action)(myValue);
				}
			}
		}

		template<std::invocable<const T&> Lfn, std::invocable<const T&> Rfn>
		inline constexpr
			monad_result_t<Lfn, const T&&>
			transform(Lfn&& safe_action, Rfn&& fail_action) const&
			noexcept(noexcept(std::forward<Lfn>(safe_action)(std::declval<const T&>())) && noexcept(std::forward<Rfn>(fail_action)(std::declval<const T&>())))
		{
			using safe_result_t = monad_result_t<Lfn, const T&>;
			using fail_result_t = monad_result_t<Rfn, const T&>;

			static_assert(std::same_as<safe_result_t, fail_result_t> || (std::same_as<safe_result_t, void> && std::same_as<fail_result_t, void>));

			if (hasValue)
			{
				if constexpr (std::is_same_v<safe_result_t, void>)
				{
					std::forward<Lfn>(safe_action)(myValue);
				}
				else
				{
					return std::forward<Lfn>(safe_action)(myValue);
				}
			}
			else
			{
				if constexpr (std::is_same_v<fail_result_t, void>)
				{
					std::forward<Rfn>(fail_action)(myValue);
				}
				else
				{
					return std::forward<Rfn>(fail_action)(myValue);
				}
			}
		}

		template<std::invocable<T&&> Lfn, std::invocable<T&&> Rfn>
		inline constexpr
			monad_result_t<Lfn, T&>
			transform(Lfn&& safe_action, Rfn&& fail_action) &&
			noexcept(noexcept(std::forward<Lfn>(safe_action)(std::declval<T&&>())) && noexcept(std::forward<Rfn>(fail_action)(std::declval<T&&>())))
		{
			using safe_result_t = monad_result_t<Lfn, T&&>;
			using fail_result_t = monad_result_t<Rfn, T&&>;

			static_assert(std::same_as<safe_result_t, fail_result_t> || (std::same_as<safe_result_t, void> && std::same_as<fail_result_t, void>));

			if (hasValue)
			{
				if constexpr (std::is_same_v<safe_result_t, void>)
				{
					std::forward<Lfn>(safe_action)(std::move(myValue));
				}
				else
				{
					return std::forward<Lfn>(safe_action)(std::move(myValue));
				}
			}
			else
			{
				if constexpr (std::is_same_v<fail_result_t, void>)
				{
					std::forward<Rfn>(fail_action)(std::move(myValue));
				}
				else
				{
					return std::forward<Rfn>(fail_action)(std::move(myValue));
				}
			}
		}

		template<std::invocable<const T&&> Lfn, std::invocable<const T&&> Rfn>
		inline constexpr
			monad_result_t<Lfn, const T&&>
			transform(Lfn&& safe_action, Rfn&& fail_action) const&&
			noexcept(noexcept(std::forward<Lfn>(safe_action)(std::declval<const T&&>())) && noexcept(std::forward<Rfn>(fail_action)(std::declval<const T&&>())))
		{
			using safe_result_t = monad_result_t<Lfn, const T&&>;
			using fail_result_t = monad_result_t<Rfn, const T&&>;

			static_assert(std::same_as<safe_result_t, fail_result_t> || (std::same_as<safe_result_t, void> && std::same_as<fail_result_t, void>));

			if (hasValue)
			{
				if constexpr (std::is_same_v<safe_result_t, void>)
				{
					std::forward<Lfn>(safe_action)(std::move(myValue));
				}
				else
				{
					return std::forward<Lfn>(safe_action)(std::move(myValue));
				}
			}
			else
			{
				if constexpr (std::is_same_v<fail_result_t, void>)
				{
					std::forward<Rfn>(fail_action)(std::move(myValue));
				}
				else
				{
					return std::forward<Rfn>(fail_action)(std::move(myValue));
				}
			}
		}

		template<std::invocable<T&> Fn, typename Uty>
		inline constexpr
			Uty
			forward(Fn&& safe_action, Uty&& default_value) &
			noexcept(noexcept(std::forward<Fn>(safe_action)(std::declval<T&>())))
		{
			using fn_result_t = monad_result_t<Fn, T&>;

			static_assert(std::convertible_to<fn_result_t, Uty>);

			if (hasValue)
			{
				return std::forward<Fn>(safe_action)(myValue);
			}
			else
			{
				return std::forward<Uty>(default_value);
			}
		}

		template<std::invocable<const T&> Fn, typename Uty>
		inline constexpr
			Uty
			forward(Fn&& safe_action, Uty&& default_value) const&
			noexcept(noexcept(std::forward<Fn>(safe_action)(std::declval<const T&>())))
		{
			using fn_result_t = monad_result_t<Fn, const T&>;

			static_assert(std::convertible_to<fn_result_t, Uty>);

			if (hasValue)
			{
				return std::forward<Fn>(safe_action)(myValue);
			}
			else
			{
				return std::forward<Uty>(default_value);
			}
		}

		template<std::invocable<T&&> Fn, typename Uty>
		inline constexpr
			Uty
			forward(Fn&& safe_action, Uty&& default_value) &&
			noexcept(noexcept(std::forward<Fn>(safe_action)(std::declval<T&&>())))
		{
			using fn_result_t = monad_result_t<Fn, T&&>;

			static_assert(std::convertible_to<fn_result_t, Uty>);

			if (hasValue)
			{
				return std::forward<Fn>(safe_action)(std::move(myValue));
			}
			else
			{
				return std::forward<Uty>(default_value);
			}
		}

		template<std::invocable<const T&&> Fn, typename Uty>
		inline constexpr
			Uty
			forward(Fn&& safe_action, Uty&& default_value) const&&
			noexcept(noexcept(std::forward<Fn>(safe_action)(std::declval<const T&&>())))
		{
			using fn_result_t = monad_result_t<Fn, const T&&>;

			static_assert(std::convertible_to<fn_result_t, Uty>);

			if (hasValue)
			{
				return std::forward<Fn>(safe_action)(std::move(myValue));
			}
			else
			{
				return std::forward<Uty>(default_value);
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&, Args...>
		inline constexpr
			Monad&
			else_than(Fn&& action, Args&&... args) &
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&>(), std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&, Args...>
		inline constexpr
			const Monad&
			else_than(Fn&& action, Args&&... args) const&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&>(), std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&&, Args...>
		inline constexpr
			Monad&&
			else_than(Fn&& action, Args&&... args) &&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&&>(), std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&&, Args...>
		inline constexpr
			const Monad&&
			else_than(Fn&& action, Args&&... args) const&&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&&>(), std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&, Args...>
		inline constexpr
			auto
			or_else(Fn&& action, Args&&... args) &
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, T&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (!hasValue)
			{
				return std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&, Args...>
		inline constexpr
			auto
			or_else(Fn&& action, Args&&... args) const&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, const T&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (!hasValue)
			{
				return std::forward<Fn>(action)(myValue, std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, T&&, Args...>
		inline constexpr
			auto
			or_else(Fn&& action, Args&&... args) &&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<T&&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, T&&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (!hasValue)
			{
				return std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, const T&&, Args...>
		inline constexpr
			auto
			or_else(Fn&& action, Args&&... args) const&&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<const T&&>(), std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, const T&&, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			using fwd_value_t = fwd_result_t::value_type;

			if (!hasValue)
			{
				return std::forward<Fn>(action)(std::move(myValue), std::forward<Args>(args)...);
			}
			else
			{
				return Monad<fwd_value_t>{ std::nullopt };
			}
		}

		constexpr T& value() & noexcept
		{
			return myValue;
		}

		constexpr const T& value() const& noexcept
		{
			return myValue;
		}

		constexpr T&& value() && noexcept(std::is_nothrow_move_constructible_v<T>)
		{
			return std::move(myValue);
		}

		constexpr const T&& value() const&& noexcept(std::is_nothrow_move_constructible_v<const T>)
		{
			return std::move(myValue);
		}

		constexpr T value_or(const T& failsafe) const& noexcept(std::is_nothrow_constructible_v<T>)
		{
			if (hasValue)
			{
				return myValue;
			}
			else
			{
				return failsafe;
			}
		}

		constexpr T value_or(T&& failsafe) const& noexcept(std::is_nothrow_move_constructible_v<T>)
		{
			if (hasValue)
			{
				return myValue;
			}
			else
			{
				return failsafe;
			}
		}

		constexpr T value_or(const T& failsafe) && noexcept(std::is_nothrow_constructible_v<T>)
		{
			if (hasValue)
			{
				return std::move(myValue);
			}
			else
			{
				return (failsafe);
			}
		}

		constexpr T&& value_or(T&& failsafe) && noexcept(std::is_nothrow_move_constructible_v<T>)
		{
			if (hasValue)
			{
				return std::move(myValue);
			}
			else
			{
				return std::move(failsafe);
			}
		}

		constexpr T get() const& noexcept(std::is_nothrow_copy_constructible_v<T>)
		{
			return myValue;
		}

		constexpr T&& get() && noexcept(std::is_nothrow_move_constructible_v<T>)
		{
			return std::move(myValue);
		}

		constexpr bool has_value() const noexcept
		{
			return hasValue;
		}

		explicit constexpr operator T () const noexcept(std::is_nothrow_copy_constructible_v<T>)
		{
			return myValue;
		}

		explicit constexpr operator bool() const noexcept
		{
			return hasValue;
		}

		constexpr Monad() noexcept(std::is_nothrow_default_constructible_v<T>) = default;
		constexpr Monad(const Monad&) noexcept(std::is_nothrow_copy_constructible_v<T>) = default;
		constexpr Monad(Monad&&) noexcept(std::is_nothrow_move_constructible_v<T>) = default;
		constexpr Monad& operator=(const Monad&) noexcept(std::is_nothrow_copy_assignable_v<T>) = default;
		constexpr Monad& operator=(Monad&&) noexcept(std::is_nothrow_move_assignable_v<T>) = default;
		constexpr ~Monad() noexcept(std::is_nothrow_destructible_v<T>) = default;

	private:
		T myValue = {};
		bool hasValue = false;
	};

	template<>
	class [[nodiscard]] Monad<void>
	{
	public:
		constexpr Monad(std::nullopt_t) noexcept {}

		template<std::copyable Uty>
		constexpr Monad(Monad<Uty>& other) noexcept
			: hasValue(other.hasValue)
		{}

		template<std::copyable Uty>
		constexpr Monad(const Monad<Uty>& other) noexcept
			: hasValue(other.hasValue)
		{}

		template<std::copyable Uty>
		constexpr Monad(Monad<Uty>&& other) noexcept
			: hasValue(std::move(other.hasValue))
		{}

		template<std::copyable Uty>
		constexpr Monad(const Monad<Uty>&& other) noexcept
			: hasValue(std::move(other.hasValue))
		{}

		template<std::copyable Uty>
		constexpr Monad& operator=(Monad<Uty>& other) noexcept
		{
			hasValue = other.hasValue;
			return *this;
		}

		template<std::copyable Uty>
		constexpr Monad& operator=(const Monad<Uty>& other) noexcept
		{
			hasValue = other.hasValue;
			return *this;
		}

		template<std::copyable Uty>
		constexpr Monad& operator=(Monad<Uty>&& other) noexcept
		{
			hasValue = std::move(other.hasValue);
			return *this;
		}

		template<std::copyable Uty>
		constexpr Monad& operator=(const Monad<Uty>&& other) noexcept
		{
			hasValue = std::move(other.hasValue);
			return *this;
		}

		constexpr Monad& operator=(Monad&&) noexcept = default;

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr Monad&
			if_then(Fn&& action, Args&&... args) &
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr const Monad&
			if_then(Fn&& action, Args&&... args) const&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr Monad&&
			if_then(Fn&& action, Args&&... args) &&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return std::move(*this);
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr const Monad&&
			if_then(Fn&& action, Args&&... args) const&&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return std::move(*this);
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr
			monad_result_t<Fn, Args...>
			and_then(Fn&& action, Args&&... args)
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			if (hasValue)
			{
				return std::forward<Fn>(action)(std::forward<Args>(args)...);
			}
			else
			{
				return fwd_result_t{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr
			monad_result_t<Fn, Args...>
			and_then(Fn&& action, Args&&... args) const
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			if (hasValue)
			{
				return std::forward<Fn>(action)(std::forward<Args>(args)...);
			}
			else
			{
				return fwd_result_t{ std::nullopt };
			}
		}

		template<std::invocable Lfn, std::invocable Rfn>
		inline constexpr
			monad_result_t<Lfn>
			transform(Lfn&& safe_action, Rfn&& fail_action)
			noexcept(noexcept(std::forward<Lfn>(safe_action)()) && noexcept(std::forward<Rfn>(fail_action)()))
		{
			using safe_result_t = monad_result_t<Lfn>;
			using fail_result_t = monad_result_t<Rfn>;

			static_assert(std::same_as<safe_result_t, fail_result_t> || (std::same_as<safe_result_t, void> && std::same_as<fail_result_t, void>));

			if (hasValue)
			{
				if constexpr (std::is_same_v<safe_result_t, void>)
				{
					std::forward<Lfn>(safe_action)();
				}
				else
				{
					return std::forward<Lfn>(safe_action)();
				}
			}
			else
			{
				if constexpr (std::is_same_v<fail_result_t, void>)
				{
					std::forward<Rfn>(fail_action)();
				}
				else
				{
					return std::forward<Rfn>(fail_action)();
				}
			}
		}

		template<std::invocable Lfn, std::invocable Rfn>
		inline constexpr
			monad_result_t<Lfn>
			transform(Lfn&& safe_action, Rfn&& fail_action) const
			noexcept(noexcept(std::forward<Lfn>(safe_action)()) && noexcept(std::forward<Rfn>(fail_action)()))
		{
			using safe_result_t = monad_result_t<Lfn>;
			using fail_result_t = monad_result_t<Rfn>;

			static_assert(std::same_as<safe_result_t, fail_result_t> || (std::same_as<safe_result_t, void> && std::same_as<fail_result_t, void>));

			if (hasValue)
			{
				if constexpr (std::is_same_v<safe_result_t, void>)
				{
					std::forward<Lfn>(safe_action)();
				}
				else
				{
					return std::forward<Lfn>(safe_action)();
				}
			}
			else
			{
				if constexpr (std::is_same_v<fail_result_t, void>)
				{
					std::forward<Rfn>(fail_action)();
				}
				else
				{
					return std::forward<Rfn>(fail_action)();
				}
			}
		}

		template<std::invocable Fn, typename Uty>
		inline constexpr
			Uty
			forward(Fn&& safe_action, Uty&& default_value)
			noexcept(noexcept(std::forward<Fn>(safe_action)()))
		{
			using fn_result_t = monad_result_t<Fn>;

			static_assert(std::convertible_to<fn_result_t, Uty>);

			if (hasValue)
			{
				return std::forward<Fn>(safe_action)();
			}
			else
			{
				return std::forward<Uty>(default_value);
			}
		}

		template<std::invocable Fn, typename Uty>
		inline constexpr
			Uty
			forward(Fn&& safe_action, Uty&& default_value) const
			noexcept(noexcept(std::forward<Fn>(safe_action)()))
		{
			using fn_result_t = monad_result_t<Fn>;

			static_assert(std::convertible_to<fn_result_t, Uty>);

			if (hasValue)
			{
				return std::forward<Fn>(safe_action)();
			}
			else
			{
				return std::forward<Uty>(default_value);
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr Monad&
			else_than(Fn&& action, Args&&... args) &
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr const Monad&
			else_than(Fn&& action, Args&&... args) const&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return *this;
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr Monad&&
			else_than(Fn&& action, Args&&... args) &&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return std::move(*this);
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr const Monad&&
			else_than(Fn&& action, Args&&... args) const&&
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			if (!hasValue)
			{
				std::forward<Fn>(action)(std::forward<Args>(args)...);
			}

			return std::move(*this);
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr
			monad_result_t<Fn, Args...>
			or_else(Fn&& action, Args&&... args)
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			if (!hasValue)
			{
				return std::forward<Fn>(action)(std::forward<Args>(args)...);
			}
			else
			{
				return fwd_result_t{ std::nullopt };
			}
		}

		template<typename Fn, typename... Args>
			requires std::invocable<Fn, Args...>
		inline constexpr
			monad_result_t<Fn, Args...>
			or_else(Fn&& action, Args&&... args) const
			noexcept(noexcept(std::forward<Fn>(action)(std::declval<Args>()...)))
		{
			using fwd_result_t = monad_result_t<Fn, Args...>;

			static_assert(!std::is_same_v<fwd_result_t, void>);
			static_assert(is_specialization_v<fwd_result_t, Monad>);

			if (!hasValue)
			{
				return std::forward<Fn>(action)(std::forward<Args>(args)...);
			}
			else
			{
				return fwd_result_t{ std::nullopt };
			}
		}

		constexpr Monad() noexcept = default;
		constexpr Monad(const Monad&) noexcept = default;
		constexpr Monad(Monad&&) noexcept = default;
		constexpr ~Monad() noexcept = default;

		bool hasValue = false;
	};

	template<typename T>
	Monad(T) -> Monad<T>;
}

export namespace util::tests
{
	void do_monad_something() noexcept
	{}

	void test_monad() noexcept
	{
		Monad<int> monad1{};
		const Monad<int> monad2{};
		constexpr Monad<int> monad3{};

		Monad<int> monad4{ 1000 };
		const bool has4 = monad4.has_value();
		const Monad<int> monad5{ 1000 };
		const bool has5 = monad5.has_value();
		constexpr Monad<int> monad6{ 1000 };
		constexpr bool has6 = monad6.has_value();

		Monad<int> monad7{ 80264.01954f };
		const Monad<int> monad8{ false };
		constexpr Monad<int> monad9{ 1000ULL };

		const auto valor7 = monad7.value_or(42);

		const auto expr8_3 = monad8.transform(
			[](const int& v) noexcept -> float { return static_cast<unsigned>(v); },
			[](const int& v) noexcept -> float { return v * 100.0f; }
		);

		const auto expr8_4 = monad8.forward(
			[](auto&&) noexcept -> long long { return 15718742LL; },
			-7
		);

		const auto& expr8_5 = monad8.else_than(
			[](auto&&) noexcept { do_monad_something(); }
		);

		constexpr int valor3 = monad3.value_or(3000.0);

		constexpr const auto& expr9_1 = monad9.if_then(
			[](auto&&) noexcept -> float { return 3000.0; }
		);

		constexpr util::Monad<float> expr9_2 = monad9.and_then(
			[](auto&&) noexcept -> util::Monad<float> {
			return { 3000.0 };
		});

		constexpr double expr9_3 = monad9.transform(
			[](const int& value) noexcept -> double { return value + 3000.0; },
			[](auto&&) noexcept -> double { return 0.0; }
		);

		constexpr int expr9_4 = monad9.forward(
			[](auto&&) noexcept -> long long { return 15718742LL; },
			-7
		);

		// not `noexcept`
		constexpr const util::Monad<int>& expr9_5 = monad9.else_than(
			[](auto&&) {}
		);

		// `noexcept`
		constexpr const util::Monad<int>& expr9_6 = monad9.else_than(
			[](auto&&) noexcept -> void {}
		);

		constexpr util::Monad<float> expr9_7 = monad9.or_else(
			[](const int& v) -> util::Monad<float> {
			return 5020.0f;
		});
	}
}
