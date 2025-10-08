/*
*
#include <stdio.h>
#include "shell.h"

void process_instruction()
{
    * execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
/*
#include <stdio.h>
#include <stdint.h>
#include "shell.h"

void process_instruction() {

    uint32_t instruction = mem_read_32(CURRENT_STATE.PC);

    uint32_t opcode     = (instruction >> 26) & 0x3F;
    uint32_t rs         = (instruction >> 21) & 0x1F;
    uint32_t rt         = (instruction >> 16) & 0x1F;
    int16_t immediate   = instruction & 0xFFFF;

    printf("Fetched instruction: 0x%08x\n", instruction);
    printf("Opcode: 0x%x, rs: $%d, rt: $%d, immediate: %d\n", opcode, rs, rt, immediate);

    switch (opcode) {
        case 0x09:
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + immediate;
            break;

        case 0x0C:  
            if (CURRENT_STATE.REGS[2] == 10) {
                RUN_BIT = FALSE;  
            }
            break;

        default:
            printf("Unknown or unimplemented instruction with opcode: 0x%x\n", opcode);
            RUN_BIT = FALSE;  
            break;
    }

       NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

*/

/*
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/



#include <stdio.h>
#include <stdint.h>
#include "shell.h"

/*jump type instruction*/
#define J 	0x02
#define JAL  	0x03


/*branch instruction */
#define BEQ   	0x04
#define BNE   	0x05
#define BLEZ  	0x06
#define BGTZ  	0x07
/*rt register*/
#define REGIMM  0x01
#define BLTZ  	0x00
#define BGEZ  	0x01
#define BLTZAL	0x10
#define BGEZAL	0x11


/*immediate type instruction*/ 
#define ADDIU 	0x09
#define ADDI  	0x08
#define SLTI  	0x0A
#define SLTIU 	0x0B
#define ANDI  	0x0C
#define ORI   	0x0D
#define XORI  	0x0E

#define LUI   	0x0F
#define LW    	0x23
#define LB    	0x20
#define LH    	0x21
#define LBU   	0x24
#define LHU   	0x25
#define SB    	0x28
#define SH    	0x29
#define SW	0x2B


/*register type instruction*/
#define SLL    	0x00
#define SRL    	0x02
#define SRA    	0x03
#define SLLV   	0x04
#define SRLV   	0x06
#define SRAV  	0x07
#define JR     	0x08
#define JALR   	0x09
#define ADD    	0x20
#define ADDU   	0x21
#define SUB    	0x22
#define SUBU   	0x23
#define AND    	0x24
#define OR     	0x25
#define XOR    	0x26
#define NOR    	0x27
#define SLT    	0x2A
#define SLTU   	0x2B
#define MULT   	0x18
#define MULTU  	0x19
#define DIV    	0x1A
#define DIVU   	0x1B
#define MFHI   	0x10
#define MFLO   	0x12
#define MTHI   	0x11
#define MTLO   	0x13
#define SYSCALL	0x0C



void R_TYPE(uint32_t instruction);
void I_TYPE(uint32_t instruction, uint32_t opcode);
void J_TYPE(uint32_t instruction);

void process_instruction() {
	uint32_t instruction = mem_read_32(CURRENT_STATE.PC);
	uint32_t opcode = (instruction >> 26) & 0x3F;

 	printf("\n=== Fetching Instruction ===\n");
	printf("PC = 0x%08x | Instruction = 0x%08x | Opcode = 0x%02x\n", CURRENT_STATE.PC, instruction, opcode);

    	NEXT_STATE.PC = CURRENT_STATE.PC + 4;

 	if(opcode == 0x00){
		R_TYPE(instruction);
    	}else if (opcode == 0x02 || opcode == 0x03){
		J_TYPE(instruction);
    	}else{
        	I_TYPE(instruction,opcode);
    	}

}

void R_TYPE(uint32_t instruction){
	
	uint32_t rs = (instruction >> 21) & 0x1F;
	uint32_t rt = (instruction >> 16) & 0x1F;
	uint32_t rd = (instruction >> 11) & 0x1F;
	uint32_t shamt = (instruction >> 6) & 0x1F;
    	uint32_t funct = instruction & 0x3F;

	switch (funct) {
			case ADD:   NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rs] + (int32_t)CURRENT_STATE.REGS[rt];
				    printf("ADD: R[%d] = R[%d] + R[%d]\n", rd, rs, rt); 
			       	    break;
        		case ADDU:  NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
				    printf("ADDU: R[%d] = R[%d] + R[%d]\n", rd, rs, rt);
			            break;
        		case SUB:   NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rs] - (int32_t)CURRENT_STATE.REGS[rt]; 
				    printf("SUB: R[%d] = R[%d] - R[%d]\n", rd, rs, rt);
			            break;
        		case SUBU:  NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt]; 
				    printf("SUBU: R[%d] = R[%d] - R[%d]\n", rd, rs, rt);
			            break;
        		case AND:   NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
				    printf("AND: R[%d] = R[%d] & R[%d]\n", rd, rs, rt); 
			    	    break;
        		case OR:    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]; 
				    printf("OR: R[%d] = R[%d] | R[%d]\n", rd, rs, rt);
			            break;
        		case XOR:   NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt]; 
				    printf("XOR: R[%d] = R[%d] ^ R[%d]\n", rd, rs, rt);
			            break;
        		case NOR:   NEXT_STATE.REGS[rd] = ~(CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]); 
				    printf("NOR: R[%d] = ~(R[%d] | R[%d])\n", rd, rs, rt);
	   		            break;
        		case SLT:   NEXT_STATE.REGS[rd] = ((int32_t)CURRENT_STATE.REGS[rs] < (int32_t)CURRENT_STATE.REGS[rt]); 
				    printf("SLT: R[%d] = (R[%d] < R[%d])\n", rd, rs, rt);
			            break;
        		case SLTU:  NEXT_STATE.REGS[rd] = (CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt]); 
				    printf("SLTU: R[%d] = (R[%d] < R[%d])\n", rd, rs, rt);
			            break;
        		case MULT:{
			           int64_t result = (int32_t)CURRENT_STATE.REGS[rs] * (int32_t)CURRENT_STATE.REGS[rt];
			 	   printf("MULT: HI/LO = R[%d] * R[%d] | currentstateregs[rs] = %x  |  currentstateregs[rt] =  %x rseult = %ld \n", rs, rt,CURRENT_STATE.REGS[rs],CURRENT_STATE.REGS[rt],result );
         			   NEXT_STATE.LO = result & 0xFFFFFFFF;
            			   NEXT_STATE.HI = (int64_t)result << 32;
				   break;
        		  	  }
        		case MULTU:{
				    printf("MULTU: HI/LO = R[%d] * R[%d]\n", rs, rt);
            		       	    uint64_t result = (uint32_t)CURRENT_STATE.REGS[rs] * (uint32_t)CURRENT_STATE.REGS[rt];
            			    NEXT_STATE.HI = (result >> 32) & 0xFFFFFFFF;
            			    NEXT_STATE.LO = result & 0xFFFFFFFF;
            			    break;
        		   	   }
         	 	case DIV:   printf("DIV: LO = R[%d] / R[%d], HI = R[%d] %% R[%d]\n", rs, rt, rs, rt);
				    if (CURRENT_STATE.REGS[rt] != 0) { 
								      NEXT_STATE.LO = (int32_t)CURRENT_STATE.REGS[rs] / (int32_t)CURRENT_STATE.REGS[rt]; 
								      NEXT_STATE.HI = (int32_t)CURRENT_STATE.REGS[rs] % (int32_t)CURRENT_STATE.REGS[rt]; 
							     	     } 
		            	    break;
        		case DIVU:  printf("DIVU: LO = R[%d] / R[%d], HI = R[%d] %% R[%d]\n", rs, rt, rs, rt);
				    if (CURRENT_STATE.REGS[rt] != 0) {  		 
								      NEXT_STATE.LO = CURRENT_STATE.REGS[rs] / CURRENT_STATE.REGS[rt]; 
								      NEXT_STATE.HI = CURRENT_STATE.REGS[rs] % CURRENT_STATE.REGS[rt]; 
							             } 
			    	    break;
        		case MFHI:  NEXT_STATE.REGS[rd] = CURRENT_STATE.HI; 
				    printf("MFHI: R[%d] = HI\n", rd);
			    	    break;
        		case MFLO:  NEXT_STATE.REGS[rd] = CURRENT_STATE.LO; 
				    printf("MFLO: R[%d] = LO\n", rd);
			    	    break;
        		case MTHI:  NEXT_STATE.HI = CURRENT_STATE.REGS[rs]; 
				    printf("MTHI: HI = R[%d]\n", rs);
			    	    break;
        		case MTLO:  NEXT_STATE.LO = CURRENT_STATE.REGS[rs]; 
				    printf("MTLO: LO = R[%d]\n", rs);
			            break;
        		case SLL:   NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
				    printf("SLL: R[%d] = R[%d] << %d\n", rd, rt, shamt);
			            break;
        		case SRL:   NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt; 
				    printf("SRL: R[%d] = R[%d] >> %d\n", rd, rt, shamt);
			            break;
        		case SRA:   NEXT_STATE.REGS[rd] = ((int32_t)CURRENT_STATE.REGS[rt]) >> shamt;
				    printf("SRA: R[%d] = R[%d] >> %d \n", rd, rt, shamt); 
			            break;
        		case SLLV:  NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << CURRENT_STATE.REGS[rs];
				    printf("SLLV: R[%d] = R[%d] << (R[%d] & 0x1F)\n", rd, rt, rs); 
			    	    break;
        		case SRLV:  NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> CURRENT_STATE.REGS[rs];
				    printf("SRLV: R[%d] = R[%d] >> (R[%d] & 0x1F)\n", rd, rt, rs);
			            break;
        		case SRAV:  NEXT_STATE.REGS[rd] = ((int32_t)CURRENT_STATE.REGS[rt]) >> CURRENT_STATE.REGS[rs]; 
				    printf("SRAV: R[%d] = R[%d] >> (R[%d] & 0x1F)\n", rd, rt, rs);
			            break;
       			case JR:    NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
				    printf("JR: PC = R[%d]\n", rs); 
			    	    break;
         		case JALR:  NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4; 
				    NEXT_STATE.PC = CURRENT_STATE.REGS[rs]; 
				    printf("JALR: R[%d] = PC + 4; PC = R[%d]\n", rd, rs);
			    	    break;
        		case SYSCALL: if (CURRENT_STATE.REGS[2] == 10) RUN_BIT = FALSE; 
			              break;
        		default:    printf("Unknown R-type funct: 0x%x\n", funct); 
			            
        }
}


void J_TYPE(uint32_t instruction) {
   	uint32_t opcode = (instruction >> 26) & 0x3F;
    	uint32_t address = instruction & 0x03FFFFFF;
    	uint32_t jump_target = (CURRENT_STATE.PC & 0xF0000000) | (address << 2);

    	switch (opcode) {
        		case J:   NEXT_STATE.PC = jump_target; 
				  printf("JUMP to 0x%08x\n", jump_target);
			  	  return;
        		case JAL: NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4; 
			  	  NEXT_STATE.PC = jump_target; 
				  printf("JAL to 0x%08x, storing return address in R[31]\n", jump_target);
			  	  return;
        		default:  printf("Unknown J-type opcode: 0x%x\n", opcode); 
			  	  
        }
}


void I_TYPE(uint32_t instruction, uint32_t opcode) {
   	
	uint32_t rs = (instruction >> 21) & 0x1F;
    	uint32_t rt = (instruction >> 16) & 0x1F;
    	int16_t imm = instruction & 0xFFFF;
	int32_t simm = (int32_t)imm;

    	switch (opcode) {
        		case ADDIU: NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + simm;
				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;	 
				    printf("ADDIU: R[%d] = R[%d] + %d\n", rt, rs, simm);
			    	    break;
        		case ADDI:  NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + simm;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4; 
				    printf("ADDI: R[%d] = R[%d] + %d\n", rt, rs, simm);
			            break;
        		case SLTI:  NEXT_STATE.REGS[rt] = (int32_t)CURRENT_STATE.REGS[rs] < simm;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("SLTI: R[%d] = (R[%d] < %d)\n", rt, rs, simm);
				    break;
			case SLTIU: NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] < (uint32_t)imm;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("SLTIU: R[%d] = (R[%d] < %u)\n", rt, rs, (uint16_t)imm);	
				    break;
			case ANDI:  NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & (uint32_t)imm;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("ANDI: R[%d] = R[%d] & 0x%X\n", rt, rs, (uint16_t)imm);
				    break;
			case ORI:   NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | (uint32_t)imm;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("ORI: R[%d] = R[%d] | 0x%X\n", rt, rs, (uint16_t)imm);
				    break;
			case XORI:  NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ (uint32_t)imm;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("XORI: R[%d] = R[%d] ^ 0x%X\n", rt, rs, (uint16_t)imm);
				    break;
   			case LUI:   NEXT_STATE.REGS[rt] = (int32_t)imm << 16;
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("LUI: R[%d] = 0x%X << 16\n", rt, (uint16_t)imm);
				    break;
			case LB:    NEXT_STATE.REGS[rt] = (int8_t)mem_read_32(CURRENT_STATE.REGS[rs] + (int16_t)imm);
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("LB: R[%d] = MEM[R[%d] + %d] (byte)\n", rt, rs, simm);
				    break;
			case LBU:   NEXT_STATE.REGS[rt] = (uint8_t)mem_read_32(CURRENT_STATE.REGS[rs] + (int16_t)imm);
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("LBU: R[%d] = MEM[R[%d] + %d] (unsigned byte)\n", rt, rs, simm);
				    break;
			case LH:    NEXT_STATE.REGS[rt] = (int16_t)mem_read_32(CURRENT_STATE.REGS[rs] + (int16_t)imm);
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("LH: R[%d] = MEM[R[%d] + %d] (halfword)\n", rt, rs, simm);
				    break;
			case LHU:   NEXT_STATE.REGS[rt] = (uint16_t)mem_read_32(CURRENT_STATE.REGS[rs] + (int16_t)imm);
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("LHU: R[%d] = MEM[R[%d] + %d] (unsigned halfword)\n", rt, rs, simm);
				    break;
			case LW:    NEXT_STATE.REGS[rt] = mem_read_32(CURRENT_STATE.REGS[rs] + (int16_t)imm);
    				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("LW: R[%d] = MEM[R[%d] + %d] (word)\n", rt, rs, simm);
				    break;  
			case SB:    mem_write_32(CURRENT_STATE.REGS[rs] + (int32_t)imm, CURRENT_STATE.REGS[rt] & 0x000000FF);//may be correct
				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("SB: MEM[R[%d] + %d] = R[%d] (byte)\n", rs, simm, rt);		 
				    break;
 			case SH:    mem_write_32(CURRENT_STATE.REGS[rs] + (int32_t)imm, CURRENT_STATE.REGS[rt] & 0x0000FFFF);//may be correct
				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("SH: MEM[R[%d] + %d] = R[%d] (halfword)\n", rs, simm, rt);		 
				    break;
  			case SW:    mem_write_32(CURRENT_STATE.REGS[rs] + (int32_t)imm, CURRENT_STATE.REGS[rt]);//may be correct
				    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
				    printf("SW: MEM[R[%d] + %d] = R[%d] (word)\n", rs, simm, rt);		 
				    break;
 			case BEQ:   if(CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]){
				    							 NEXT_STATE.PC = CURRENT_STATE.PC + ((int16_t)imm << 2);
											 printf("BEQ: Branch taken - R[%d] == R[%d], offset = %d\n", rs, rt, simm);
				    } 
				    else {
				    	  NEXT_STATE.PC = CURRENT_STATE.PC + 4;	
					  printf("BEQ: Branch not taken - R[%d] != R[%d]\n", rs, rt);
				    }					
  				    break;
 			case BNE:   if(CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]){
				    							 NEXT_STATE.PC = CURRENT_STATE.PC + ((int16_t)imm << 2);
											 printf("BNE: Branch taken - R[%d] != R[%d], offset = %d\n", rs, rt, simm);
				    }
				    else {	
					  NEXT_STATE.PC = CURRENT_STATE.PC + 4;
					  printf("BNE: Branch not taken - R[%d] == R[%d]\n", rs, rt);
				    }
				    break;
 			case BLEZ:  if(CURRENT_STATE.REGS[rs] <= 0){
				    				    NEXT_STATE.PC = CURRENT_STATE.PC + ((int16_t)imm << 2);
								    printf("BLEZ: Branch taken - R[%d] <= 0, offset = %d\n", rs, simm);
				    }
			    	    else {
                                          NEXT_STATE.PC = CURRENT_STATE.PC + 4;
					  printf("BLEZ: Branch not taken - R[%d] > 0\n", rs);
				    }
				    break;
 			case BGTZ:  if(CURRENT_STATE.REGS[rs] > 0){
				    				   NEXT_STATE.PC = CURRENT_STATE.PC + ((int16_t)imm << 2);
								   printf("BGTZ: Branch taken - R[%d] > 0, offset = %d\n", rs, simm);
				    }
				    else {		
    					  NEXT_STATE.PC = CURRENT_STATE.PC + 4;
					  printf("BGTZ: Branch not taken - R[%d] <= 0\n", rs);
				    } 
				    break;
			case REGIMM: 
				    switch (rt){	
 					 	case BLTZ:  if(CURRENT_STATE.REGS[rs] < 0){
											   NEXT_STATE.PC = CURRENT_STATE.PC + 4 + ((int16_t)imm << 2);
											   printf("BLTZ: Branch taken - R[%d] < 0, offset = %d\n", rs, simm);
							    }  
							    else {
                                          			  NEXT_STATE.PC = CURRENT_STATE.PC + 4;
								  printf("BLTZ: Branch not taken - R[%d] >= 0\n", rs);
				    			    }
				 	         	    break;
 				  		case BGEZ:  if(CURRENT_STATE.REGS[rs] >= 0){
											    NEXT_STATE.PC = CURRENT_STATE.PC + 4 + ((int16_t)imm << 2);
											    printf("BGEZ: Branch taken - R[%d] >= 0, offset = %d\n", rs, simm);
							    }
 							    else {
                                          			  NEXT_STATE.PC = CURRENT_STATE.PC + 4;
								  printf("BGEZ: Branch not taken - R[%d] < 0\n", rs);
				    			    }
				  	       		    break;
 				  		case BLTZAL:if(CURRENT_STATE.REGS[rs] < 0){
											   NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
											   NEXT_STATE.PC = CURRENT_STATE.PC + 4 + ((int16_t)imm << 2);
											   printf("BLTZAL: Branch taken - R[%d] < 0, link = %u\n", rs, CURRENT_STATE.PC + 4);
							    }
 							    else {
                                          			  NEXT_STATE.PC = CURRENT_STATE.PC + 4;
								  printf("BLTZAL: Branch not taken - R[%d] >= 0\n", rs);
				     			    }
				  	       		    break;
 				  		case BGEZAL:if(CURRENT_STATE.REGS[rs] >= 0){
											    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4; 
											    NEXT_STATE.PC = CURRENT_STATE.PC + 4 + ((int16_t)imm << 2);
											    printf("BGEZAL: Branch taken - R[%d] >= 0, link = %u\n", rs, CURRENT_STATE.PC + 4);
							    }
   							    else {
                                          			  NEXT_STATE.PC = CURRENT_STATE.PC + 4;
								  printf("BGEZAL: Branch not taken - R[%d] < 0\n", rs);
				     			    }
				  	       		    break;
						default:    printf("Unknown REGIMM rt field: %d\n",rt);
							    break;
						}
				    break;
	
        		default:  printf("Unknown I-type opcode: 0x%x\n", opcode); 
			  	  
        }
}

