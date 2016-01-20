///
/// @file   PushBackPrimes.hpp
/// @brief  This file contains classes needed to store primes in
///         std::vector objects. These classes derive from Callback
///         and call PrimeSieve's callbackPrimes() method, the
///         primes are then pushed back onto the vector inside the
///         callback method.
///
/// Copyright (C) 2015 Kim Walisch, <kim.walisch@gmail.com>
///
/// This file is distributed under the BSD License. See the COPYING
/// file in the top level directory.
///

#ifndef PUSHBACKPRIMES_HPP
#define PUSHBACKPRIMES_HPP

#include "PrimeSieve.hpp"
#include "Callback.hpp"
#include "cancel_callback.hpp"

#include <stdint.h>
#include <cmath>

namespace primesieve {

/// approximate_prime_count(x) >= pi(x)
inline uint64_t approximate_prime_count(uint64_t start, uint64_t stop)
{
  if (start > stop)
    return 0;
  if (stop <= 10)
    return 4;

  // Dusard 2010: pi(x) <= x / (log(x) - 1.1) + 5 for x >= 4
  double pix = (stop - start) / (std::log(static_cast<double>(stop)) - 1.1) + 5;

  return static_cast<uint64_t>(pix);
}

template <typename T>
class PushBackPrimes : public Callback<uint64_t>
{
public:
  PushBackPrimes(T& primes)
    : primes_(primes)
  { }

  void callback(uint64_t prime)
  {
    typedef typename T::value_type V;
    primes_.push_back(static_cast<V>(prime));
  }

  void pushBackPrimes(uint64_t start, uint64_t stop)
  {
    if (start <= stop)
    {
      uint64_t prime_count = approximate_prime_count(start, stop);
      primes_.reserve(primes_.size() + static_cast<std::size_t>(prime_count));
      PrimeSieve ps;
      ps.callbackPrimes(start, stop, this);
    }
  }
private:
  PushBackPrimes(const PushBackPrimes&);
  void operator=(const PushBackPrimes&);
  T& primes_;
};

template <typename T>
class PushBack_N_Primes : public Callback<uint64_t>
{
public:
  PushBack_N_Primes(T& primes) 
    : primes_(primes)
  { }

  void callback(uint64_t prime)
  {
    typedef typename T::value_type V;
    primes_.push_back(static_cast<V>(prime));
    if (--n_ == 0)
      throw cancel_callback();
  }

  void pushBack_N_Primes(uint64_t n, uint64_t start)
  {
    n_ = n;
    std::size_t newSize = primes_.size() + static_cast<std::size_t>(n_);
    primes_.reserve(newSize);
    PrimeSieve ps;
    try
    {
      while (n_ > 0)
      {
        uint64_t logn = 50;
        // choose stop > nth prime
        uint64_t stop = start + n_ * logn + 10000;
        ps.callbackPrimes(start, stop, this);
        start = stop + 1;
      }
    }
    catch (cancel_callback&) { }
  }
private:
  PushBack_N_Primes(const PushBack_N_Primes&);
  void operator=(const PushBack_N_Primes&);
  T& primes_;
  uint64_t n_;
};

} // namespace primesieve

#endif
