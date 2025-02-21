//		ui512aTests
// 
//		File:			ui512aTests.cpp
//		Author:			John G.Lynch
//		Legal:			Copyright @2024, per MIT License below
//		Date:			May 13, 2024
//
//		ui512 is a small project to provide basic operations for a variable type of unsigned 512 bit integer.
//		The basic operations : zero, copy, compare, add, subtract.
//		Other optional modules provide bit ops and multiply / divide.
//		It is written in assembly language, using the MASM ( ml64 ) assembler provided as an option within Visual Studio.
//		( currently using VS Community 2022 17.9.6 )
//		It provides external signatures that allow linkage to C and C++ programs,
//		where a shell / wrapper could encapsulate the methods as part of an object.
//		It has assembly time options directing the use of Intel processor extensions : AVX4, AVX2, SIMD, or none :
//		( Z ( 512 ), Y ( 256 ), or X ( 128 ) registers, or regular Q ( 64bit ) ).
//		If processor extensions are used, the caller must align the variables declared and passed
//		on the appropriate byte boundary ( e.g. alignas 64 for 512 )
//		This module is very light - weight ( less than 1K bytes ) and relatively fast,
//		but is not intended for all processor types or all environments.
//		Use for private ( hobbyist ), or instructional,
//		or as an example for more ambitious projects is all it is meant to be.
//
//		This sub - project: ui512aTests, is a unit test project that invokes each of the routines in the ui512a assembly.
//		It runs each assembler proc with pseudo - random values.
//		It validates ( asserts ) expected and returned results.
//		It also runs each repeatedly for comparative timings.
//		It provides a means to invoke and debug.
//		It illustrates calling the routines from C++.

#include "pch.h"
#include "CppUnitTest.h"

#include <format>

#include "ui512a.h"

using namespace std;
using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace ui512aTests
{
	TEST_CLASS(ui512aTests)
	{
	public:
		const s32 runcount = 2500;
		const s32 timingcount = 100000000;

		/// <summary>
		/// Random number generator
		/// uses linear congruential method 
		/// ref: Knuth, Art Of Computer Programming, Vol. 2, Seminumerical Algorithms, 3rd Ed. Sec 3.2.1
		/// </summary>
		/// <param name="seed">if zero, will supply with: 4294967291</param>
		/// <returns>Pseudo-random number from zero to ~2^63 (9223372036854775807)</returns>
		u64 RandomU64(u64* seed)
		{
			const u64 m = 9223372036854775807ull;			// 2^63 - 1, a Mersenne prime
			const u64 a = 68719476721ull;					// closest prime below 2^36
			const u64 c = 268435399ull;						// closest prime below 2^28
			// suggested seed: around 2^32, 4294967291
			*seed = (*seed == 0ull) ? (a * 4294967291ull + c) % m : (a * *seed + c) % m;
			return *seed;
		};

		TEST_METHOD(random_number_generator)
		{
			//	Check distibution of "random" numbers
			u64 seed = 0;
			const u32 dec = 10;
			u32 dist[dec]{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

			const u64 split = 9223372036854775807ull / dec;
			u32 distc = 0;
			float varsum = 0.0;
			float deviation = 0.0;
			float D = 0.0;
			float sumD = 0.0;
			float varience = 0.0;
			const u32 randomcount = 1000000;
			const s32 norm = randomcount / dec;
			for (u32 i = 0; i < randomcount; i++)
			{
				seed = RandomU64(&seed);
				dist[u64(seed / split)]++;
			};

			string msgd = "Evaluation of pseudo-random number generator.\n\n";
			msgd += format("Generated {0:*>8} numbers.\n", randomcount);
			msgd += format("Counted occurances of those numbers by decile, each decile {0:*>20}.\n", split);
			msgd += format("Distribution of numbers accross the deciles indicates the quality of the generator.\n\n");
			msgd += "Distribution by decile:";
			string msgv = "Variance from mean:\t";
			string msgchi = "Varience ^2 (chi):\t";

			for (int i = 0; i < 10; i++)
			{
				deviation = float(abs(long(norm) - long(dist[i])));
				D = (deviation * deviation) / float(long(norm));
				sumD += D;
				varience = float(deviation) / float(norm) * 100.0f;
				varsum += varience;
				msgd += format("\t{:6d}", dist[i]);
				msgv += format("\t{:5.3f}% ", varience);
				msgchi += format("\t{:5.3f}% ", D);
				distc += dist[i];
			};

			msgd += "\t\tDecile counts sum to: " + to_string(distc) + "\n";
			Logger::WriteMessage(msgd.c_str());
			msgv += "\t\tVarience sums to: ";
			msgv += format("\t{:6.3f}% ", varsum);
			msgv += '\n';
			Logger::WriteMessage(msgv.c_str());
			msgchi += "\t\tChi distribution: ";
			msgchi += format("\t{:6.3f}% ", sumD);
			msgchi += '\n';
			Logger::WriteMessage(msgchi.c_str());
		};

		TEST_METHOD(ui512a_01_zero)
		{
			// Test zero function. Set var to some random number, call function. Test to see if zero. Repeat. 
			u64 seed = 0;
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = RandomU64(&seed);
				};

				zero_u(num1);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(0ull, num1[j]);
				};
			};

			string runmsg = "Zero function testing. Ran tests " + to_string(runcount) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert.\n\n");
		};

		TEST_METHOD(ui512a_01_zero_timing)
		{
			// Zero function timing. Run function a bunch of times. See if coding changes improve/reduce overall timing.
			// Eliminate everthing possible except just repeatedly calling the function.
			u64 seed = 0;
			alignas (64) u64 num1[8]
			{
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed),
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed)
			};

			for (int i = 0; i < timingcount; i++)
			{
				zero_u(num1);
			};

			for (int j = 0; j < 8; j++)
			{
				Assert::AreEqual(0ull, num1[j]);
			};

			string runmsg = "Zero function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_02_copy)
		{
			// Test copy function. Set var to some random number, copy it to another (zeroed) var. Test second var to see if equal. Repeat.
			u64 seed = 0;
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 num2[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = RandomU64(&seed);
					num2[j] = 0;
				};

				copy_u(num2, num1);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual((const u64)num1[j], (const u64)num2[j]);
				};
			};

			string runmsg = "Copy function testing. Ran tests " + to_string(runcount) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert.\n\n");
		};

		TEST_METHOD(ui512a_02_copy_timing)
		{
			// Copy function timing. Run function a bunch of times. See if coding changes improve/reduce overall timing.
			// Eliminate everthing possible except just repeatedly calling the function.
			u64 seed = 0;
			alignas (64) u64 num1[8]
			{
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed),
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed)
			};
			alignas (64) u64 num2[8]{ 8, 7, 6, 5, 4, 3, 2, 1 };

			for (int i = 0; i < timingcount; i++)
			{
				copy_u(num2, num1);
			};

			for (int j = 0; j < 8; j++)
			{
				Assert::AreEqual((const u64)num1[j], (const u64)num2[j]);
			};

			string runmsg = "Copy function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_03_set64)
		{
			// Test set function. Set var to some random number, set it to some random (64bit) var. Test first var to see if equal. Repeat.
			u64 seed = 0;
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 val = 0;

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = RandomU64(&seed);
				};

				val = RandomU64(&seed);
				set_uT64(num1, val);

				for (int j = 0; j < 7; j++)
				{
					Assert::AreEqual((const u64)num1[j], (const u64)0);
				};

				Assert::AreEqual((const u64)num1[7], (const u64)val);;
			};
			string runmsg = "Set value function testing. Ran tests " + to_string(runcount) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert.\n\n");
		};

		TEST_METHOD(ui512a_03_set64_timing)
		{
			// Set function timing. Run function a bunch of times. See if coding changes improve/reduce overall timing.
			// Eliminate everthing possible except just repeatedly calling the function.
			u64 seed = 0;
			alignas (64) u64 num1[8]
			{
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed),
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed)
			};
			u64 val = RandomU64(&seed);

			for (int i = 0; i < timingcount; i++)
			{
				set_uT64(num1, val);
			};

			for (int j = 0; j < 7; j++)
			{
				Assert::AreEqual((const u64)num1[j], (const u64)0);
			}

			Assert::AreEqual((const u64)num1[7], (const u64)val);
			string runmsg = "Set value (x64) function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_04_compare)
		{
			u64 seed = 0;
			s32 eval;
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 num2[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = num2[j] = RandomU64(&seed);
				};

				for (int j = 0; j <= 7; j++)
				{
					num2[j]++;
					eval = compare_u(num1, num2);
					Assert::AreEqual(eval, -1);
					num2[j] -= 2;
					eval = compare_u(num1, num2);
					Assert::AreEqual(eval, 1);
					num2[j]++;
					eval = compare_u(num1, num2);
					Assert::AreEqual(eval, 0);
				};
			};

			string runmsg = "Compare function testing. Ran tests " + to_string(3 * 8 * runcount) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert\n\n.");
		};

		TEST_METHOD(ui512a_04_compare_timing)
		{
			u64 seed = 0;
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 num2[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };

			for (int j = 0; j < 8; j++)
			{
				num1[j] = num2[j] = RandomU64(&seed);
			};

			s32 eval = 999;
			for (int i = 0; i < timingcount; i++)
			{
				eval = compare_u(num1, num2);
			};

			Assert::AreEqual(eval, 0);
			string runmsg = "Compare function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_05_compare64)
		{
			u64 seed = 0;
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 8 };
			// test that higher order words are checked
			u64 num2 = 8;
			for (int i = 0; i < 7; i++)
			{
				num1[i] = RandomU64(&seed);
				s32 eval;
				eval = compare_uT64(num1, num2);
				Assert::AreEqual(eval, 1);
			};

			for (int i = 0; i < runcount; i++)
			{
				alignas (64) u64 num[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
				num[7] = num2 = RandomU64(&seed);
				for (int j = 0; j <= 7; j++)
				{
					s32 eval;
					num2++;
					eval = compare_uT64(num, num2);
					Assert::AreEqual(eval, -1);
					num2 -= 2;
					eval = compare_uT64(num, num2);
					Assert::AreEqual(eval, 1);
					num2++;
					eval = compare_uT64(num, num2);
					Assert::AreEqual(eval, 0);
				};
			};

			string runmsg = "Compare (x64) function testing. Ran tests " + to_string(3 * 8 * runcount) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert\n\n.");
		};

		TEST_METHOD(ui512a_05_compare64_timing)
		{
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 num2 = 0;
			u64 seed = 0;

			num1[7] = num2 = RandomU64(&seed);
			s32 eval = 999;
			for (int i = 0; i < timingcount; i++)
			{
				eval = compare_uT64(num1, num2);
			};

			Assert::AreEqual(eval, 0);
			string runmsg = "Compare (T64) function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_06_add)
		{
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 num2[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 sum[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 one[8]{ 0, 0, 0, 0, 0, 0, 0, 1 };
			u64 seed = 0;
			s32 overflow = 0;

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = RandomU64(&seed);
					num2[j] = ~num1[j];
					sum[j] = 0;
				};
				// add test: "random" number plus ones complement should equal 0xfff..., no carries or overflow
				overflow = add_u(sum, num1, num2);
				Assert::AreEqual(overflow, 0);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(sum[j], 0xFFFFFFFFFFFFFFFFull);
				};
				// now add one, should cascade carries through all eight, making them each zero, and overflow
				overflow = add_u(sum, sum, one);		// Note:  Destination (sum) is also an operand
				Assert::AreEqual(overflow, 1);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(sum[j], 0x0000000000000000ull);
				};

				// run same tests, with destination being one of the sources
				for (int j = 0; j < 8; j++)
				{
					num1[j] = RandomU64(&seed);
					num2[j] = ~num1[j];
				};
				// add test: "random" number plus ones complement should equal 0xfff..., no carries or overflow
				overflow = add_u(num1, num1, num2);
				Assert::AreEqual(overflow, 0);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(num1[j], 0xFFFFFFFFFFFFFFFFull);
				};
				// now add one, should cascade carries through all eight, making them each zero, and overflow
				overflow = add_u(num1, num1, one);		// Note:  Destination (sum) is also an operand
				Assert::AreEqual(overflow, 1);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(num1[j], 0x0000000000000000ull);
				};
			};

			string runmsg = "Add function testing. Ran tests " + to_string(runcount * 5) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert\n\n.");
		};

		TEST_METHOD(ui512a_06_add_timing)
		{
			u64 seed = 0;
			alignas (64) u64 num1[8]
			{
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed),
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed)
			};
			alignas (64) u64 num2[8]
			{
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed),
				RandomU64(&seed), RandomU64(&seed), RandomU64(&seed), RandomU64(&seed)
			};
			alignas (64) u64 sum[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };

			for (int i = 0; i < timingcount; i++)
			{
				add_u(sum, num1, num2);
			};

			string runmsg = "Add function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_07_add64)
		{
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 sum[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 one[8]{ 0, 0, 0, 0, 0, 0, 0, 1 };
			u64 seed = 0;
			u64 num2 = 0;
			s32 overflow = 0;

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = 0xFFFFFFFFFFFFFFFFull;
					sum[j] = 0;
				};
				num2 = RandomU64(&seed);
				num1[7] = ~num2;
				// add test: "random" number plus ones complement should equal 0xfff..., no carries or overflow
				overflow = add_uT64(sum, num1, num2);
				Assert::AreEqual(0, overflow);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(0xFFFFFFFFFFFFFFFFull, sum[j]);
				};
				// now add one, should cascade carries through all eight, making them each zero, and overflow
				overflow = add_uT64(sum, sum, 0x0000000000000001ull);		// Note:  Destination (sum) is also an operand
				Assert::AreEqual(1, overflow);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(0x0000000000000000ull, sum[j]);
				};
			};

			string runmsg = "Add (x64) function testing. Ran tests " + to_string(runcount * 2) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert\n\n.");
		};

		TEST_METHOD(ui512a_07_add64_timimg)
		{
			alignas (64) u64 num1[8]{ 1, 2, 3, 4, 5, 6, 7, 8 };
			alignas (64) u64 sum[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 seed = 0;
			u64 num2 = 8;

			for (int i = 0; i < timingcount; i++)
			{
				add_uT64(sum, num1, num2);
			};

			string runmsg = "Add (x64) function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_08_subtract)
		{
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 num2[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 diff[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 one[8]{ 0, 0, 0, 0, 0, 0, 0, 1 };
			u64 seed = 0;
			s32 borrow = 0;

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = num2[j] = RandomU64(&seed);
					diff[j] = 0;
				};
				// subtract test: "random" number minus same number equals zero..., no borrow
				borrow = sub_u(diff, num1, num2);
				Assert::AreEqual(borrow, 0);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(diff[j], 0x0000000000000000ull);
				};
				// now subtract one, should cascade borrows through all eight, making them each 0xFFF... , and overflow to borrow
				borrow = sub_u(diff, diff, one);		// Note:  Destination (sum) is also an operand
				Assert::AreEqual(borrow, 1);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(diff[j], 0xFFFFFFFFFFFFFFFFull);
				};
			};

			string runmsg = "Subtract function testing. Ran tests " + to_string(runcount * 2) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert\n\n.");
		};

		TEST_METHOD(ui512a_08_subtract_timing)
		{
			u64 seed = 0;

			alignas (64) u64 num1[8]{ 1, 2, 3, 4, 5, 6, 7, 8 };
			alignas (64) u64 num2[8]{ 1, 2, 3, 4, 5, 6, 7, 8 };
			alignas (64) u64 diff[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			for (int i = 0; i < timingcount; i++)
			{
				sub_u(diff, num1, num2);
			};

			string runmsg = "Subtract function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};

		TEST_METHOD(ui512a_09_subtract64)
		{
			alignas (64) u64 num1[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			alignas (64) u64 diff[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 num2 = 0;
			u64 one = 1;
			u64 seed = 0;
			s32 borrow = 0;

			for (int i = 0; i < runcount; i++)
			{
				for (int j = 0; j < 8; j++)
				{
					num1[j] = 0;
					diff[j] = 0;
				};
				// subtract test: "random" number minus same number equals zero..., no borrow
				num1[7] = num2 = RandomU64(&seed);
				num2 = num1[7];
				borrow = sub_uT64(diff, num1, num2);
				Assert::AreEqual(borrow, 0);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(diff[j], 0x0000000000000000ull);
				};
				// now subtract one, should cascade borrows through all eight, making them each 0xFFF... , and overflow to borrow
				borrow = sub_uT64(diff, diff, one);		// Note:  Destination (sum) is also an operand
				Assert::AreEqual(borrow, 1);
				for (int j = 0; j < 8; j++)
				{
					Assert::AreEqual(diff[j], 0xFFFFFFFFFFFFFFFFull);
				};
			};

			string runmsg = "Subtract (T64) function testing. Ran tests " + to_string(runcount * 2) + " times, each with pseudo random values.\n";
			Logger::WriteMessage(runmsg.c_str());
			Logger::WriteMessage(L"Passed. Tested expected values via assert\n\n.");
		};

		TEST_METHOD(ui512a_09_subtract64_timing)
		{
			u64 seed = 0;

			alignas (64) u64 num1[8]{ 1, 2, 3, 4, 5, 6, 7, 8 };
			u64 num2 = 8;
			alignas (64) u64 diff[8]{ 0, 0, 0, 0, 0, 0, 0, 0 };
			for (int i = 0; i < timingcount; i++)
			{
				sub_uT64(diff, num1, num2);
			};

			string runmsg = "Subtract (x64) function timing. Ran " + to_string(timingcount) + " times.\n";
			Logger::WriteMessage(runmsg.c_str());
		};
	};
}
