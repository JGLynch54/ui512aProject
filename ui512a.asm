;
;			ui512a
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;
;			File:			ui512a.asm
;			Author:			John G. Lynch
;			Legal:			Copyright @2024, per MIT License below
;			Date:			May 13, 2024
;
;			Notes:
;				ui512 is a small project to provide basic operations for a variable type of unsigned 512 bit integer.
;
;				ui512a provides basic operations: zero, copy, compare, add, subtract.
;				ui512b provides basic bit-oriented operations: shift left, shift right, and, or, not, least significant bit and most significant bit.
;               ui512md provides multiply and divide.
;
;				It is written in assembly language, using the MASM (ml64) assembler provided as an option within Visual Studio.
;				(currently using VS Community 2022 17.9.6)
;
;				It provides external signatures that allow linkage to C and C++ programs,
;				where a shell/wrapper could encapsulate the methods as part of an object.
;
;				It has assembly time options directing the use of Intel processor extensions: AVX4, AVX2, SIMD, or none:
;				(Z (512), Y (256), or X (128) registers, or regular Q (64bit)).
;
;				If processor extensions are used, the caller must align the variables declared and passed
;				on the appropriate byte boundary (e.g. alignas 64 for 512)
;
;				This module is very light-weight (less than 1K bytes) and relatively fast,
;				but is not intended for all processor types or all environments. 
;
;				Use for private (hobbyist), or instructional, or as an example for more ambitious projects is all it is meant to be.
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

			INCLUDE			ui512aMacros.inc
			OPTION			casemap:none
.CODE
			OPTION			PROLOGUE:none
			OPTION			EPILOGUE:none
			
ret0		DD				0
ret1		DD				1
ret_1		DD				-1
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			zero_u		-	fill supplied 512bit (8 QWORDS) with zero
;			Prototype:		extern "C" void zero_u ( u64* destarr );
;			destarr		-	Address of destination 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			returns		-	nothing
;

zero_u		PROC			PUBLIC
			Zero512			RCX
			RET		
zero_u		ENDP 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			copy_u		-	copy supplied 512bit (8 QWORDS) source to supplied destination
;			Prototype:		extern "C" void copy_u( u64* destarr, u64* srcarr )
;			destarr		-	Address of destination 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			srcarr		-	Address of source 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	nothing

copy_u		PROC			PUBLIC
			Copy512			RCX, RDX
			RET	
copy_u		ENDP

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			setuT64		-	set supplied destination 512 bit to supplied u64 value
;			Prototype:		extern "C" void set_uT64( u64* destarr, u64 value )
;			destarr		-	Address of destination 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			src			-	u64 value in RDX
;			returns		-	nothing

set_uT64	PROC			PUBLIC
			Zero512			RCX	
			MOV				Q_PTR [ RCX ] + [ 7 * 8 ], RDX
			RET	
set_uT64	ENDP

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			compare_u	-	compare supplied 512bit (8 QWORDS) LH operand to supplied RH operand
;			Prototype:		extern "C" s32 compare_u( u64* lh_op, u64* rh_op )
;			lh_op		-	Address of LH 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	Address of RH 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	(0) for equal, -1 for less than, 1 for greater than
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore

compare_u	PROC			PUBLIC

	IF		__UseZ
			VMOVDQA64		ZMM30, ZM_PTR [ RCX ]
			VMOVDQA64		ZMM31, ZM_PTR [ RDX ]
			VPCMPUQ			K1, ZMM30, ZMM31, CPLT
			KMOVB			EAX, K1
			VPCMPUQ			K2, ZMM30, ZMM31, CPGT			
			KMOVB			ECX, K2
			CMP				CX, AX
			CMOVE			EAX, ret0

	ELSEIF	__UseY
			VMOVDQA64		YMM0, YM_PTR [ RCX ] + [ 4 * 8 ]
			VMOVDQA64		YMM1, YM_PTR [ RCX ] + [ 0 * 8 ]
			VMOVDQA64		YMM2, YM_PTR [ RDX ] + [ 4 * 8 ]
			VMOVDQA64		YMM3, YM_PTR [ RDX ] + [ 0 * 8 ]
			VPCMPUQ			K1, YMM0, YMM2, CPLT
			KMOVB			EAX, K1
			VPCMPUQ			K2, YMM1, YMM3, CPLT
			KMOVB			EDX, K2
			SHL				EAX, 4
			OR				EAX, EDX
			VPCMPUQ			K3, YMM0, YMM2, CPGT
			KMOVB			EDX, K3
			VPCMPUQ			K4, YMM1, YMM3, CPGT
			KMOVB			ECX, K4
			SHL				EDX, 4
			OR				EDX, ECX
			CMP				EDX, EAX
			CMOVE			EAX, ret0

	ELSE
			MOV				RAX, [ RCX ] + [ 0 * 8 ]
			CMP				RAX, [ RDX ] + [ 0 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 1 * 8 ]
			CMP				RAX, [ RDX ] + [ 1 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 2 * 8 ]
			CMP				RAX, [ RDX ] + [ 2 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 3 * 8 ]
			CMP				RAX, [ RDX ] + [ 3 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 4 * 8 ]
			CMP				RAX, [ RDX ] + [ 4 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 5 * 8 ]
			CMP				RAX, [ RDX ] + [ 5 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 6 * 8 ]
			CMP				RAX, [ RDX ] + [ 6 * 8 ]
			JNZ				@F
			MOV				RAX, [ RCX ] + [ 7 * 8 ]
			CMP				RAX, [ RDX ] + [ 7 * 8 ]
			JNZ				@F
			XOR				EAX, EAX
@@:
	ENDIF
			CMOVG			EAX, ret1
			CMOVL			EAX, ret_1
			RET
compare_u	ENDP 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			compare_uT64-	compare supplied 512bit (8 QWORDS) LH operand to supplied 64bit RH operand
;			Prototype:		extern "C" s32 compare_uT64( u64* lh_op, u64 rh_op )
;			lh_op		-	Address of LH 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	The RH 64-bit value in RDX
;			returns		-	(0) for equal, -1 for less than, 1 for greater than
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore

compare_uT64 PROC			PUBLIC

	IF		__UseZ
			VMOVDQA64		ZMM30, ZM_PTR [ RCX ]
			LEA				RAX, MaskBit7
			KMOVB			k1, RAX
			VPBROADCASTQ 	ZMM31 {k1}{z}, RDX
			VPCMPUQ			K1, ZMM30, ZMM31, CPLT
			KMOVB			EAX, K1
			VPCMPUQ			K2, ZMM30, ZMM31, CPGT			
			KMOVB			ECX, K2
			CMP				CX, AX
			CMOVE			EAX, ret0

	ELSE
			XOR				RAX, RAX
			CMP				Q_PTR [ RCX ] + [ 0 * 8 ], RAX
			JNZ				@F
			CMP				Q_PTR [ RCX ] + [ 1 * 8 ], RAX
			JNZ				@F
			CMP				Q_PTR [ RCX]  + [ 2 * 8 ], RAX
			JNZ				@F
			CMP				Q_PTR [ RCX ] + [ 3 * 8 ], RAX
			JNZ				@F
			CMP				Q_PTR [ RCX ] + [ 4 * 8 ], RAX
			JNZ				@F
			CMP				Q_PTR [ RCX ] + [ 5 * 8 ], RAX
			JNZ				@F
			CMP				Q_PTR [ RCX ] + [ 6 * 8 ], RAX
			JNZ				@F	
			MOV				RAX, [ RCX ] + [ 7 * 8 ]
			CMP				RAX, RDX 
			JNZ				@F
			XOR				EAX, EAX
@@:

	ENDIF
			CMOVG			EAX, ret1
			CMOVL			EAX, ret_1
			RET
compare_uT64 ENDP

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			add_u		-	add supplied 512bit (8 QWORDS) sources to supplied destination
;			Prototype:		extern "C" s32 add_u( u64* sum, u64* addend1, u64* addend2 )
;			sum			-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore

add_u		PROC			PUBLIC 
	IF		__PrefZ
	;		PrefZ : Prefer ZMM Registers
	;		Testing whether reducing memory read / writes by using the ZMM registers as scratch memory is faster
	;		This routine, and the corresponding routine without the prefer Zregs setting, tests that premise
	;
	;		Preferring ZMM registers:
	;			Load the two input 8-word values into two ZMM regs (one memory fetch for each (aligned))
	;			Extract each corresponding addend word into GP regs, do the addition, put the sum into another Z reg
	;			When finished with all eight, store the resulting sum (one Z memory write)
	;			Other than the inital two loads, and the ending store, there are no memory ops, no bus ops, 
	;			all ops are within the CPU and in theory at CPU clock speeds as opposed to bus / memory speeds
	;			Unknowns: 
	;				how big is the instruction pipeline? are we waiting for memory fetches for instructions (no branches)?
	;				How much clock downshifting occurs when using the ZMM regs (and instructions)?
	;
	;		After testing, using the Z regs in this way is significantly slower than using GP regs, even though many more memory accesses
	;		Note: This is not using the Z regs for their intended purpose: SIMD; this is using them as awkward scratch registers
	;				Using the Z regs for SIMD, such as in the compare: the Z (SIMD) approach is faster than GP approach


			VMOVDQA64		ZMM28, ZM_PTR [ RDX ]
			VMOVDQA64		ZMM29, ZM_PTR [ R8 ]

			LEA				RAX, MaskBit7				; lowest order word ( 7 of 0-7 )
			KMOVB			K1, RAX
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADD				RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 6 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 5 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 4 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 3 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 2 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 1 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			KSHIFTRB		K1, K1, 1					; 0 of 0-7
			VPCOMPRESSQ		ZMM0 {k1}{z}, ZMM28
			VMOVQ			RAX, XMM0
			VPCOMPRESSQ		ZMM1 {k1}{z}, ZMM29
			VMOVQ			RDX, XMM1
			ADCX			RAX, RDX
			VPBROADCASTQ 	ZMM30 {k1}, RAX

			VMOVDQA64		ZM_PTR [ RCX ], ZMM30		; store result

	ELSEIF	__PrefQ
	;		PrefQ : Prefer regular GP Registers
	;		Testing whether reordering memory read / writes by doing all the reads, then the writes
	;		In theory, the writes will cause cache invalidation, thus slowing the next read
	;		This routine, and the corresponding routine without the prefer Q reg setting, tests that premise

	;		Preferring GP registers:
	;			Save 8 GP regs on stack (these are new and additional instructions)
	;			For each of 8 words: Fetch addend into a GP reg, Add other addend to it (carrying the carry bit)
	;			All memory fetches and operations are done.  so trash the cache by a sequence of 8 writes to memory.
	;			Restore the 8 GP registers from the stack (additional instructions)
	;
	;		This approach adds 10 instructions (five pushes, five pops) that are also memory writes / reads
	;				
	;		Unknowns: 
	;			how big is the instruction pipeline? are we waiting for memory fetches for instructions?
	;
	;		After testing, using the GP regs in this way is slightly slower than straight read, operate, store each word in sequence
	;
			PUSH			R15
			PUSH			R14
			PUSH			R13
			PUSH			R12
			PUSH			RBX
			MOV				RBX, R8

			MOV				R8, [ RDX ] + [ 7 * 8 ]	
			MOV				R9, [ RDX ] + [ 6 * 8 ]
			MOV				R10, [ RDX ] + [ 5 * 8 ]
			MOV				R11, [ RDX ] + [ 4 * 8 ]
			MOV				R12, [ RDX ] + [ 3 * 8 ]
			MOV				R13, [ RDX ] + [ 2 * 8 ]
			MOV				R14, [ RDX ] + [ 1 * 8 ]
			MOV				R15, [ RDX ] + [ 0 * 8 ]
			
			ADD				R8, [ RBX ] + [ 7 * 8 ]
			ADCX			R9, [ RBX ] + [ 6 * 8 ]
			ADCX			R10, [ RBX ] + [ 5 * 8 ]
			ADCX			R11, [ RBX ] + [ 4 * 8 ]
			ADCX			R12, [ RBX ] + [ 3 * 8 ]
			ADCX			R13, [ RBX ] + [ 2 * 8 ]
			ADCX			R14, [ RBX ] + [ 1 * 8 ]
			ADCX			R15, [ RBX ] + [ 0 * 8 ]

			MOV				[ RCX ] + [ 7 * 8 ], R8
			MOV				[ RCX ] + [ 6 * 8 ], R9
			MOV				[ RCX ] + [ 5 * 8 ], R10
			MOV				[ RCX ] + [ 4 * 8 ], R11
			MOV				[ RCX ] + [ 3 * 8 ], R12
			MOV				[ RCX ] + [ 2 * 8 ], R13
			MOV				[ RCX ] + [ 1 * 8 ], R14
			MOV				[ RCX ] + [ 0 * 8 ], R15

			POP				RBX
			POP				R12
			POP				R13
			POP				R14
			POP				R15

	ELSE

			MOV				RAX, [ RDX ] + [ 7 * 8 ]
			ADD				RAX, [ R8 ] + [ 7 * 8 ]
			MOV				[ RCX ] + [ 7 * 8 ], RAX

			MOV				RAX, [ RDX ] + [ 6 * 8 ]
			ADCX			RAX, [ R8 ] + [ 6 * 8 ]
			MOV				[ RCX ] + [ 6 * 8 ] , RAX

			MOV				RAX, [ RDX ] + [ 5 * 8 ]
			ADCX			RAX, [ R8 ] + [ 5 * 8 ]
			MOV				[ RCX ] + [ 5 * 8 ], RAX

			MOV				RAX, [ RDX ] + [ 4 * 8 ]
			ADCX			RAX, [ R8 ] + [ 4 * 8 ]
			MOV				[ RCX ] + [ 4 * 8 ], RAX

			MOV				RAX, [ RDX ] + [ 3 * 8 ]
			ADCX			RAX, [ R8 ] + [ 3 * 8 ]
			MOV				[ RCX ] + [ 3 * 8 ], RAX

			MOV				RAX, [ RDX ] + [ 2 * 8 ]
			ADCX			RAX, [ R8 ] + [ 2 * 8 ]
			MOV				[ RCX ] + [ 2 * 8 ], RAX

			MOV				RAX, [ RDX ] + [ 1 * 8 ]
			ADCX			RAX, [ R8 ] + [ 1 * 8 ]
			MOV				[ RCX ] + [ 1 * 8 ], RAX

			MOV				RAX, [ RDX ] + [ 0 * 8 ]
			ADCX			RAX, [ R8 ] + [ 0 * 8 ]
			MOV				[ RCX ] + [ 0 * 8 ], RAX

	ENDIF
	
			MOV				EAX, 0						; return carry flag as overflow
			CMOVC			EAX, ret1
			RET	
add_u		ENDP 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
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
			CMOVC			EAX, ret1
			RET	
add_uT64	ENDP 
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			sub_u		-	subtract supplied 512bit (8 QWORDS) RH OP from LH OP giving difference in destination
;			Prototype:		extern "C" s32 sub_u( u64* difference, u64* left operand, u64* right operand )
;			difference	-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of the LHOP 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	Address of the RHOP 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore

sub_u		PROC			PUBLIC 

			MOV				RAX, [ RDX ] + [ 7 * 8 ]		; get last word of minuend (least significant word of the number we are subtracting from) (left-hand operand)
			SUB				RAX, [ R8 ] + [ 7 * 8 ]			; subtract last word of subrahend (the number to be subtracted) (right-hand operand)
			MOV				[ RCX ] + [ 7 * 8 ], RAX		; store result in last word of difference, note: the flag 'carry' has been set to whether there has been a 'borrow'

			MOV				RAX, [ RDX ] + [ 6 * 8]			; same as above for next word, but use 'sbb' to bring the 'borrow' to this op
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

			MOV				EAX, 0							; return, set return code to zero if no remaining borrow, to one if there is a borrow
			CMOVC			EAX, ret1
			RET
sub_u		ENDP 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			sub_uT64	-	subtract supplied 64 bit right hand (64 bit value) op from left hand (512 bit) giving difference
;			Prototype:		extern "C" s32 sub_uT64( u64* difference, u64* left operand, u64 right operand )
;			difference	-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	64-bitvalue in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore

sub_uT64	PROC			PUBLIC 

			MOV				RAX, [ RDX ] + [ 7 * 8 ]		; 
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
			CMOVC			EAX, ret1
			RET
sub_uT64	ENDP 

			END