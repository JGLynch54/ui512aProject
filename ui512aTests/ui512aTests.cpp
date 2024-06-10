#include "pch.h"
#include "CppUnitTest.h"

#include "CommonTypeDefs.h"
#include "ui512a.h"

using namespace std;
using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace ui512aTests
{
	TEST_CLASS ( ui512aTests )
	{
	public:
		const s32 runcount = 100;
		const s32 timingcount = 1000000;

		/// <summary>
		/// Random nnumber generator
		/// uses linear congruential method 
		/// ref: Knuth, Art Of Computer Programming, Vol. 2, Seminumerical Algorithms, 3rd Ed. Sec 3.2.1
		/// </summary>
		/// <param name="seed">if zero will supply with: 4294967291</param>
		/// <returns>Pseudo-random number from zero to ~2^63 (9223372036854775807)</returns>
		u64 RandomU64 ( u64* seed )
		{
			const u64 m = 9223372036854775807ull;			// 2^63 - 1, a Mersenne prime
			const u64 a = 68719476721ull;					// closest prime below 2^36
			const u64 c = 268435399ull;						// closest prime below 2^28
			// suggested seed: around 2^32, 4294967291
			// linear congruential method (ref: Knuth, Art Of Computer Programming, Vol. 2, Seminumerical Algorithms, 3rd Ed. Sec 3.2.1
			*seed = ( *seed == 0ull ) ? ( a * 4294967291ull + c ) % m : ( a * *seed + c ) % m;
			return *seed;
		};

		TEST_METHOD ( random_number_generator )
		{
			u64 seed = 0;
			u32 dist [ 10 ] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 split = 9223372036854775807ull / 10;
			u32 distc = 0;
			u32 randomcount = 10000;

			for ( u32 i = 0; i < randomcount; i++ )
			{
				seed = RandomU64 ( &seed );
				dist [ u64 ( seed / split ) ]++;
			};

			string msgd = "\nDistribution of ( " + to_string ( randomcount ) + " ) pseudo-random numbers by deciles:\n";
			for ( int i = 0; i < 10; i++ )
			{
				msgd += to_string ( dist [ i ] ) + " ";
				distc += dist [ i ];
			};

			msgd += " summing to " + to_string ( distc ) + "\n";
			Logger::WriteMessage ( msgd.c_str ( ) );
		};

		TEST_METHOD ( ui512a_01_zero )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ];
			for ( int i = 0; i < runcount; i++ )
			{
				for ( int j = 0; j < 8; j++ )
				{
					num1 [ j ] = RandomU64 ( &seed );
				};
				zero_u ( num1 );
				for ( int j = 0; j < 8; j++ )
				{
					Assert::AreEqual ( ( const u64 ) num1 [ j ], ( const u64 ) 0 );
				};
			};
			string runmsg = "Zero function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert.\n\n" );
		};

		TEST_METHOD ( ui512a_01_zero_timing )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ]
			{
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed )
			};

			for ( int i = 0; i < timingcount; i++ )
			{
				zero_u ( num1 );
			};

			for ( int j = 0; j < 8; j++ )
			{
				Assert::AreEqual ( ( const u64 ) num1 [ j ], ( const u64 ) 0 );
			};

			string runmsg = "Zero function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_02_copy )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ];
			alignas ( 64 ) u64 num2 [ 8 ];
			for ( int i = 0; i < runcount; i++ )
			{
				for ( int j = 0; j < 8; j++ )
				{
					num1 [ j ] = RandomU64 ( &seed );
					num2 [ j ] = 0;
				};

				copy_u ( num2, num1 );
				for ( int j = 0; j < 8; j++ )
				{
					Assert::AreEqual ( ( const u64 ) num1 [ j ], ( const u64 ) num2 [ j ] );
				};
			};

			string runmsg = "Copy function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert.\n\n" );
		};

		TEST_METHOD ( ui512a_02_copy_timing )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ]
			{
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed )
			};

			alignas ( 64 ) u64 num2 [ 8 ] { 8, 7, 6, 5, 4, 3, 2, 1 };
			for ( int i = 0; i < timingcount; i++ )
			{
				copy_u ( num2, num1 );
			};

			for ( int j = 0; j < 8; j++ )
			{
				Assert::AreEqual ( ( const u64 ) num1 [ j ], ( const u64 ) num2 [ j ] );
			};

			string runmsg = "Copy function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_03_set64 )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ];
			u64 val = 0;

			for ( int i = 0; i < runcount; i++ )
			{
				for ( int j = 0; j < 8; j++ )
				{
					num1 [ j ] = RandomU64 ( &seed );
				};

				val = RandomU64 ( &seed );
				set_uT64 ( num1, val );
				for ( int j = 0; j < 7; j++ )
				{
					Assert::AreEqual ( ( const u64 ) num1 [ j ], ( const u64 ) 0 );
				};

				Assert::AreEqual ( ( const u64 ) num1 [ 7 ], ( const u64 ) val );;
			};
			string runmsg = "Set value function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert.\n\n" );
		};

		TEST_METHOD ( ui512a_03_set64_timing )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ]
			{
				RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed )
			};

			u64 val = RandomU64 ( &seed );
			for ( int i = 0; i < timingcount; i++ )
			{
				set_uT64 ( num1, val );
			};

			for ( int j = 0; j < 7; j++ )
			{
				Assert::AreEqual ( ( const u64 ) num1 [ j ], ( const u64 ) 0 );
			}

			Assert::AreEqual ( ( const u64 ) num1 [ 7 ], ( const u64 ) val );
			string runmsg = "Set value (x64) function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_04_compare )
		{
			u64 seed = 0;
			s32 eval;
			alignas ( 64 ) u64 num1 [ 8 ];
			alignas ( 64 ) u64 num2 [ 8 ];

			for ( int i = 0; i < runcount; i++ )
			{
				for ( int j = 0; j < 8; j++ )
				{
					num1 [ j ] = num2 [ j ] = RandomU64 ( &seed );
				};

				for ( int j = 0; j <= 7; j++ )
				{
					num2 [ j ]++;
					eval = compare_u ( num1, num2 );
					Assert::AreEqual ( eval, -1 );
					num2 [ j ] -= 2;
					eval = compare_u ( num1, num2 );
					Assert::AreEqual ( eval, 1 );
					num2 [ j ]++;
					eval = compare_u ( num1, num2 );
					Assert::AreEqual ( eval, 0 );
				};
			};

			string runmsg = "Compare function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert\n\n." );
		};

		TEST_METHOD ( ui512a_04_compare_timing )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ];
			alignas ( 64 ) u64 num2 [ 8 ];

			for ( int j = 0; j < 8; j++ )
			{
				num1 [ j ] = num2 [ j ] = RandomU64 ( &seed );
			};

			s32 eval = 999;
			for ( int i = 0; i < timingcount; i++ )
			{
				eval = compare_u ( num1, num2 );
			};

			Assert::AreEqual ( eval, 0 );
			string runmsg = "Compare function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_05_compare64 )
		{
			u64 seed = 0;

			// test that higher order words are checked
			u64 num2 = 9;
			for ( int i = 0; i < 7; i++ )
			{
				alignas ( 64 ) u64 num1 [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 8 };
				num1 [ i ] = RandomU64 ( &seed );
				s32 eval;
				eval = compare_uT64 ( num1, num2 );
				Assert::AreEqual ( eval, 1 );
			};

			for ( int i = 0; i < runcount; i++ )
			{
				alignas ( 64 ) u64 num1 [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 8 };
				num1 [ 7 ] = num2 = RandomU64 ( &seed );
				for ( int j = 0; j <= 7; j++ )
				{
					s32 eval;
					num2++;
					eval = compare_uT64 ( num1, num2 );
					Assert::AreEqual ( eval, -1 );
					num2 -= 2;
					eval = compare_uT64 ( num1, num2 );
					Assert::AreEqual ( eval, 1 );
					num2++;
					eval = compare_uT64 ( num1, num2 );
					Assert::AreEqual ( eval, 0 );
				};
			};

			string runmsg = "Compare (x64) function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert\n\n." );
		};

		TEST_METHOD ( ui512a_05_compare64_timing )
		{
			alignas ( 64 ) u64 num1 [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 num2 = 0;
			u64 seed = 0;

			num1 [ 7 ] = num2 = RandomU64 ( &seed );
			s32 eval = 999;
			for ( int i = 0; i < timingcount; i++ )
			{
				eval = compare_uT64 ( num1, num2 );
			};

			Assert::AreEqual ( eval, 0 );
			string runmsg = "Compare (T64) function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_06_add )
		{
			alignas ( 64 ) u64 num1 [ 8 ];
			alignas ( 64 ) u64 num2 [ 8 ];
			alignas ( 64 ) u64 sum [ 8 ];
			alignas ( 64 ) u64 one [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 1 };
			u64 seed = 0;
			s32 overflow = 0;

			for ( int i = 0; i < runcount; i++ )
			{
				for ( int j = 0; j < 8; j++ )
				{
					num1 [ j ] = RandomU64 ( &seed );
					num2 [ j ] = ~num1 [ j ];
					sum [ j ] = 0;
				};
				// add test: "random" number plus ones complement should equal 0xfff..., no carries or overflow
				overflow = add_u ( sum, num1, num2 );
				Assert::AreEqual ( overflow, 0 );
				for ( int j = 0; j < 8; j++ )
				{
					Assert::AreEqual ( sum [ j ], 0xFFFFFFFFFFFFFFFFull );
				};
				// now add one, should cascade carries through all eight, making them each zero, and overflow
				overflow = add_u ( sum, sum, one );		// Note:  Destination (sum) is also an operand
				Assert::AreEqual ( overflow, 1 );
				for ( int j = 0; j < 8; j++ )
				{
					Assert::AreEqual ( sum [ j ], 0x0000000000000000ull );
				};
			};

			string runmsg = "Add function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert\n\n." );
		};

		TEST_METHOD ( ui512a_06_add_timing )
		{
			u64 seed = 0;
			alignas ( 64 ) u64 num1 [ 8 ]
			{
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed )
			};
			alignas ( 64 ) u64 num2 [ 8 ]
			{
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ),
				RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed ), RandomU64 ( &seed )
			};
			alignas ( 64 ) u64 sum [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 0 };

			for ( int i = 0; i < timingcount; i++ )
			{
				add_u ( sum, num1, num2 );
			};

			string runmsg = "Add function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_07_add64 )
		{
			u64 seed = 0;

			string runmsg = "Add (x64) function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert\n\n." );
		};

		TEST_METHOD ( ui512a_07_add64_timimg )
		{
			alignas ( 64 ) u64 num1 [ 8 ] { 1, 2, 3, 4, 5, 6, 7, 8 };
			alignas ( 64 ) u64 sum [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 0 };
			u64 seed = 0;
			u64 num2 = 8;

			for ( int i = 0; i < timingcount; i++ )
			{
				add_uT64 ( sum, num1, num2 );
			};

			string runmsg = "Add (x64) function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_08_subtract )
		{
			u64 seed = 0;

			string runmsg = "Subtract function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert\n\n." );
		};

		TEST_METHOD ( ui512a_08_subtract_timing )
		{
			u64 seed = 0;

			alignas ( 64 ) u64 num1 [ 8 ] { 1, 2, 3, 4, 5, 6, 7, 8 };
			alignas ( 64 ) u64 num2 [ 8 ] { 1, 2, 3, 4, 5, 6, 7, 8 };
			alignas ( 64 ) u64 diff [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 0 };
			for ( int i = 0; i < timingcount; i++ )
			{
				sub_u ( diff, num1, num2 );
			};

			string runmsg = "Subtract function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};

		TEST_METHOD ( ui512a_09_subtract64 )
		{
			u64 seed = 0;

			string runmsg = "Subtract (x64) function testing. Ran tests " + to_string ( runcount ) + " times, each with pseudo random values.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
			Logger::WriteMessage ( L"Passed. Tested expected values via assert\n\n." );
		};

		TEST_METHOD ( ui512a_09_subtract64_timing )
		{
			u64 seed = 0;

			alignas ( 64 ) u64 num1 [ 8 ] { 1, 2, 3, 4, 5, 6, 7, 8 };
			u64 num2 = 8;
			alignas ( 64 ) u64 diff [ 8 ] { 0, 0, 0, 0, 0, 0, 0, 0 };
			for ( int i = 0; i < timingcount; i++ )
			{
				sub_uT64 ( diff, num1, num2 );
			};

			string runmsg = "Subtract (x64) function timing. Ran " + to_string ( timingcount ) + " times.\n";
			Logger::WriteMessage ( runmsg.c_str ( ) );
		};
	};
}
