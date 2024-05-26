;
;			ui512a
;
;			File:			ui512a.asm
;			Author:			John G. Lynch
;			Legal:			Copyright @2024, per MIT License below
;			Date:			May 13, 2024
;
;			Notes:
;				ui512 is a small project to provide basic operations for a variable type of unsigned 512 bit integer.
;				The basic operations: zero, copy, compare, add, subtract. Other optional modules provide bit ops and multiply / divide.
;				It is written in assembly language, using the MASM (ml64) assembler provided as an option within Visual Studio (currently using VS Community 2022 17.9.6)
;				It provides external signatures that allow linkage to C and C++ programs, where a shell/wrapper could encapsulate the methods as part of an object.
;				It has assembly time options directing the use of Intel processor extensions: AVX4, AVX2, SIMD, or none: (Z (512), Y (256), or X (128) registers, or regular Q (64bit))
;				If processor extensions are used, the caller must align the variables declared and passed on the appropriate byte boundary (e.g. alignas 64 for 512)
;				This module is very light-weight (less than 1K bytes) and relatively fast, but is not intended for all processor types or all environments. 
;				Use for private (hobbyist), or instructional, or as an example for more ambitious projects is all it is meant to be.
;
;			MIT License
;
;			Copyright (c) 2024 John G. Lynch
;
;				Permission is hereby granted, free of charge, to any person obtaining a copy
;				of this software and associated documentation files (the "Software"), to deal
;				in the Software without restriction, including without limitation the rights
;				to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;				copies of the Software, and to permit persons to whom the Software is
;				furnished to do so, subject to the following conditions:
;
;				The above copyright notice and this permission notice shall be included in all
;				copies or substantial portions of the Software.
;
;				THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;				IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;				FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;				AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;				LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;				OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;				SOFTWARE.
;

			INCLUDE			ui512aMacros.inc
			OPTION			casemap:none
.CODE
			OPTION			PROLOGUE:none
			OPTION			EPILOGUE:none

;			zero_u		-	fill supplied 512bit (8 QWORDS) with zero
;			Prototype:		extern "C" void zero_u ( u64* destarr );
;			destarr		-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			returns		-	nothing
zero_u		PROC			PUBLIC
			Zero512			RCX
			RET	
zero_u		ENDP 

;			copy_u		-	copy supplied 512bit (8 QWORDS) source to supplied destination
;			Prototype:		extern "C" void copy_u( u64* destarr, u64* srcarr )
;			destarr		-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			srcarr		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	nothing
copy_u		PROC			PUBLIC
			Copy512			RCX, RDX
			RET	
copy_u		ENDP

;			setuT64		-	set supplied destination 512 bit to supplied u64 value
;			Prototype:		extern "C" void set_uT64( u64* destarr, u64 value )
;			destarr		-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			src			-	u64 value in RDX
;			returns		-	nothing
set_uT64	PROC			PUBLIC
			Zero512			RCX	
			MOV				Q_PTR [ RCX ] + [ 7 * 8 ], RDX
			RET	
set_uT64	ENDP

;			compare_u	-	compare supplied 512bit (8 QWORDS) LH operand to supplied RH operand
;			Prototype:		extern "C" s32 compare_u( u64* lh_op, u64* rh_op )
;			lh_op		-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	(0) for equal, -1 for less than, 1 for greater than
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
compare_u	PROC			PUBLIC

	IF		__UseZ

			VMOVDQA64		ZMM30, ZM_PTR [ RCX ]
			VMOVDQA64		ZMM31, ZM_PTR [ RDX ]
			VPCMPUQ			K1, ZMM30, ZMM31, CPLT
			KMOVB			RAX, K1
			VPCMPUQ			K2, ZMM30, ZMM31, CPGT			
			KMOVB			RCX, K2
			CMP				RCX, RAX
			MOV				ECX, 0
			CMOVE			EAX, ECX
			MOV				ECX, 1
			CMOVG			EAX, ECX
			MOV				ECX, -1
			CMOVL			EAX, ECX
			RET

	ELSEIF	__UseY

			VMOVDQA64		YMM0, YM_PTR [ RCX ] + [ 4 * 8 ]
			VMOVDQA64		YMM1, YM_PTR [ RCX ] + [ 0 * 8 ]
			VMOVDQA64		YMM2, YM_PTR [ RDX ] + [ 4 * 8 ]
			VMOVDQA64		YMM3, YM_PTR [ RDX ] + [ 0 * 8 ]
			VPCMPQ			K1, YMM0, YMM2, CPLT
			KMOVQ			RAX, K1
			VPCMPQ			K2, YMM1, YMM3, CPLT
			KMOVQ			RDX, K2
			SHL				RAX, 4
			OR				RAX, RDX
			VPCMPQ			K3, YMM0, YMM2, CPGT
			KMOVQ			RDX, K3
			VPCMPQ			K4, YMM1, YMM3, CPGT
			KMOVQ			RCX, K4
			SHL				RDX, 4
			OR				RDX, RCX
			CMP				RDX, RAX
			MOV				ECX, 0
			CMOVE			EAX, ECX
			MOV				ECX, 1
			CMOVG			EAX, ECX
			MOV				ECX, -1
			CMOVL			EAX, ECX
			RET

	ELSE

			MOV				RAX, [ RCX ] + [ 0 * 8 ]
			CMP				RAX, [ RDX ] + [ 0 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 1 * 8 ]
			CMP				RAX, [ RDX ] + [ 1 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 2 * 8 ]
			CMP				RAX, [ RDX ] + [ 2 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 3 * 8 ]
			CMP				RAX, [ RDX ] + [ 3 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 4 * 8 ]
			CMP				RAX, [ RDX ] + [ 4 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 5 * 8 ]
			CMP				RAX, [ RDX ] + [ 5 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 6 * 8 ]
			CMP				RAX, [ RDX ] + [ 6 * 8 ]
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 7 * 8 ]
			CMP				RAX, [ RDX ] + [ 7 * 8 ]
			JNZ				checkGTLT
			XOR				EAX, EAX
checkGTLT:
			MOV				ECX, 1
			CMOVG			EAX, ECX
			MOV				ECX, -1
			CMOVL			EAX, ECX
			RET

	ENDIF

compare_u	ENDP 

;			compare_uT64-	compare supplied 512bit (8 QWORDS) LH operand to supplied 64bit RH operand
;			Prototype:		extern "C" s32 compare_uT64( u64* lh_op, u64 rh_op )
;			lh_op		-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	The  64-bit value in RDX
;			returns		-	(0) for equal, -1 for less than, 1 for greater than
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
compare_uT64 PROC			PUBLIC
			MOV				RAX, [ RCX ] + [ 0 * 8 ]
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 1 * 8 ]
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX]  + [ 2 * 8 ]
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 3 * 8 ]
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 4 * 8 ]
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 5 * 8 ] 
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 6 * 8 ]
			CMP				RAX, 0
			JNZ				checkGTLT
			MOV				RAX, [ RCX ] + [ 7 * 8 ]
			CMP				RAX, RDX 
			JNZ				checkGTLT
			XOR				EAX, EAX
checkGTLT:
			MOV				ECX, 1
			CMOVG			EAX, ECX
			MOV				ECX, -1
			CMOVL			EAX, ECX
			RET
compare_uT64 ENDP

;			add_u		-	add supplied 512bit (8 QWORDS) sources to supplied destination
;			Prototype:		extern "C" s32 add_u( u64* sum, u64* addend1, u64* addend2 )
;			sum			-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
add_u		PROC			PUBLIC 
			MOV				RAX, [ RDX ] + [ 7 * 8 ]
			ADD				RAX, [ R8 ] + [ 7 * 8 ]
			MOV				[ RCX ] + [ 7 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 6 * 8 ]
			ADC				RAX, [ R8 ] + [ 6 * 8 ]
			MOV				[ RCX ] + [ 6 * 8 ] , RAX
			MOV				RAX, [ RDX ] + [ 5 * 8 ]
			ADC				RAX, [ R8 ] + [ 5 * 8 ]
			MOV				[ RCX ] + [ 5 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 4 * 8 ]
			ADC				RAX, [ R8 ] + [ 4 * 8 ]
			MOV				[ RCX ] + [ 4 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 3 * 8 ]
			ADC				RAX, [ R8 ] + [ 3 * 8 ]
			MOV				[ RCX ] + [ 3 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 2 * 8 ]
			ADC				RAX, [ R8 ] + [ 2 * 8 ]
			MOV				[ RCX ] + [ 2 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 1 * 8 ]
			ADC				RAX, [ R8 ] + [ 1 * 8 ]
			MOV				[ RCX ] + [ 1 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 0 * 8 ]
			ADC				RAX, [ R8 ] + [ 0 * 8 ]
			MOV				[ RCX ] + [ 0 * 8 ], RAX
			MOV				EAX, 0
			MOV				ECX, 1
			CMOVC			EAX, ECX
			RET	
add_u		ENDP 

;			add_uT64	-	add supplied 64bit QWORD (value) to 512bit (8 QWORDS), place in supplied destination
;			Prototype:		extern "C" s32 add_uT64( u64* sum, u64* addend1, u64 addend2 )
;			sum			-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	The 64-bit value in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
add_uT64	PROC			PUBLIC 
			MOV				RAX, [ RDX ] + [ 7 * 8 ]
			ADD				RAX, R8 
			MOV				[ RCX ] + [ 7 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 6 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 6 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 5 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 5 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 4 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 4 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 3 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 3 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 2 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 2 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 1 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 1 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 0 * 8 ]
			ADC				RAX, 0
			MOV				[ RCX ] + [ 0 * 8 ], RAX
			MOV				EAX, 0
			MOV				ECX, 1
			CMOVC			EAX, ECX
			RET	
add_uT64	ENDP 

;			sub_u		-	subtract supplied 512bit (8 QWORDS) RH OP from LH OP giving difference in destination
;			Prototype:		extern "C" s32 sub_u( u64* difference, u64* left operand, u64* right operand )
;			difference	-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of the LHOP 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	Address of the RHOP 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
sub_u		PROC			PUBLIC 
			MOV				RAX, [ RDX ] + [ 7 * 8 ]
			SUB				RAX, [ R8 ] + [ 7 * 8 ]
			MOV				[ RCX ] + [ 7 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 6 * 8]
			SBB				RAX, [ R8 ] + [ 6 * 8 ]
			MOV				[ RCX ] + [ 6 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 5 * 8 ]
			SBB				RAX, [ R8 ] + [ 5 * 8 ]
			MOV				[ RCX ] + [ 5 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 4 * 8 ]
			SBB				RAX, [ R8 ] + [ 4 * 8 ]
			MOV				[ RCX ] + [ 4 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 3 * 8 ]
			SBB				RAX, [ R8 ] + [ 3 * 8 ]
			MOV				[ RCX ] + [ 3 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 2 * 8 ]
			SBB				RAX, [ R8 ] + [ 2 * 8 ]
			MOV				[ RCX ] + [ 2 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 1 * 8 ]
			SBB 			RAX, [ R8 ] + [ 1 * 8 ]
			MOV				[ RCX ] + [ 1 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 0 * 8 ]
			SBB				RAX, [ R8 ] + [ 0 * 8 ]
			MOV				[ RCX ] + [ 0 * 8 ], RAX
			MOV				EAX, 0
			MOV				ECX, 1
			CMOVC			EAX, ECX
			RET
sub_u		ENDP 

;			sub_uT64	-	subtract supplied 64 bit right hand (64 bit value) op from left hand (512 bit) giving difference
;			Prototype:		extern "C" s32 sub_uT64( u64* difference, u64* left operand, u64 right operand )
;			difference	-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	64-bitvalue in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
sub_uT64	PROC			PUBLIC 
			MOV				RAX, [ RDX ] + [ 7 * 8 ]
			SUB				RAX, R8
			MOV				[ RCX ] + [ 7 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 6 * 8 ] 
			SBB				RAX, 0
			MOV				[ RCX ] + [ 6 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 5 * 8 ]
			SBB				RAX, 0
			MOV				[ RCX ] + [ 5 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 4 * 8 ]
			SBB				RAX, 0
			MOV				[ RCX ] + [ 4 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 3 * 8 ]
			SBB				RAX, 0
			MOV				[ RCX ] + [ 3 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 2 * 8 ]
			SBB				RAX, 0
			MOV				[ RCX ] + [ 2 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 1 * 8 ]
			SBB 			RAX, 0
			MOV				[ RCX ] + [ 1 * 8 ], RAX
			MOV				RAX, [ RDX ] + [ 0 * 8 ]
			SBB				RAX, 0
			MOV				[ RCX ] + [ 0 * 8 ], RAX
			MOV				EAX, 0
			MOV				ECX, 1
			CMOVC			EAX, ECX
			RET
sub_uT64	ENDP 

			END