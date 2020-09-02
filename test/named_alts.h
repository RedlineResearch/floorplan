static const char *const __FLP_TYPES[] = {"FLP_UNMAPPED", "FLP_VariableHeader", "FLP_VariableHeader_Small", "FLP_VariableHeader_Large", "FLP_FixedHeader",
                                          "FLP_FixedHeader_Mini", "FLP_FixedHeader_Big"};
#define __FLP_Entry unsigned short int
#define FLP_UNMAPPED ((__FLP_Entry)0)
#define FLP_VariableHeader ((__FLP_Entry)1)
#define FLP_VariableHeader_Small ((__FLP_Entry)2)
#define FLP_VariableHeader_Large ((__FLP_Entry)3)
#define FLP_FixedHeader ((__FLP_Entry)4)
#define FLP_FixedHeader_Mini ((__FLP_Entry)5)
#define FLP_FixedHeader_Big ((__FLP_Entry)6)
#define __FLP_NUM_VALID_TYPES ((__FLP_Entry)7)
#define FLP_ALL_VariableHeader FLP_VariableHeader, FLP_VariableHeader_Small, FLP_VariableHeader_Large
#define FLP_ALL_Small FLP_VariableHeader_Small
#define FLP_ALL_Large FLP_VariableHeader_Large
#define FLP_ALL_FixedHeader FLP_FixedHeader, FLP_FixedHeader_Mini, FLP_FixedHeader_Big
#define FLP_ALL_Mini FLP_FixedHeader_Mini
#define FLP_ALL_Big FLP_FixedHeader_Big
#define base_transition_FLP_VariableHeader(qname_type,addr,size_in_bytes)  \
  {   ShadowAddr __flp_addr1 = (ShadowAddr)addr; \
        size_t __flp_local_start0 = __flp_bytes_consumed; \
    if (is_small) { \
      base_transition_FLP_Small(qname_type##_VariableHeader,__flp_addr1,size_in_bytes); } \
    else if (!(is_small)) { \
      base_transition_FLP_Large(qname_type##_VariableHeader,__flp_addr1,size_in_bytes);  }  }
#define base_transition_FLP_Small(qname_type,addr,size_in_bytes)  \
    ShadowAddr __flp_addr2 = (ShadowAddr)addr; \
    {   size_t __flp_local_start3 = __flp_bytes_consumed; \
        { PSet(0, __flp_addr2 + (__flp_bytes_consumed - __flp_local_start3), FLP##qname_type##_Small, 8); \
            __flp_bytes_consumed += 8;    } \
        {     size_t __flp_local_sz = size_in_bytes - ((0 + 8) + 0); \
      PSet(0, __flp_addr2 + (__flp_bytes_consumed - __flp_local_start3), FLP##qname_type##_Small, __flp_local_sz); \
            __flp_bytes_consumed += __flp_local_sz;    }  }
#define base_transition_FLP_Large(qname_type,addr,size_in_bytes)  \
    ShadowAddr __flp_addr4 = (ShadowAddr)addr; \
    {   size_t __flp_local_start5 = __flp_bytes_consumed; \
        { PSet(0, __flp_addr4 + (__flp_bytes_consumed - __flp_local_start5), FLP##qname_type##_Large, 16); \
            __flp_bytes_consumed += 16;    } \
        {     size_t __flp_local_sz = size_in_bytes - ((0 + 16) + 0); \
      PSet(0, __flp_addr4 + (__flp_bytes_consumed - __flp_local_start5), FLP##qname_type##_Large, __flp_local_sz); \
            __flp_bytes_consumed += __flp_local_sz;    }  }
#define base_transition_FLP_FixedHeader(qname_type,addr)  \
  {   ShadowAddr __flp_addr7 = (ShadowAddr)addr; \
        size_t __flp_local_start6 = __flp_bytes_consumed; \
    if (is_mini) { \
      base_transition_FLP_Mini(qname_type##_FixedHeader,__flp_addr7); } \
    else if (!(is_mini)) { \
      base_transition_FLP_Big(qname_type##_FixedHeader,__flp_addr7);  }  }
#define base_transition_FLP_Mini(qname_type,addr)  \
    ShadowAddr __flp_addr8 = (ShadowAddr)addr; \
    { PSet(0, __flp_addr8, FLP##qname_type##_Mini, 8); \
        __flp_bytes_consumed += 8;  }
#define base_transition_FLP_Big(qname_type,addr)  \
    ShadowAddr __flp_addr9 = (ShadowAddr)addr; \
    { PSet(0, __flp_addr9, FLP##qname_type##_Big, 16); \
        __flp_bytes_consumed += 16;  }
#define transition_FLP_VariableHeader(addr,size_in_bytes)  \
  {   size_t __flp_bytes_consumed = 0; \
    base_transition_FLP_VariableHeader(,addr,size_in_bytes);  }
#define transition_FLP_VariableHeader_Small(addr,size_in_bytes)  \
  {   size_t __flp_bytes_consumed = 0; \
    base_transition_FLP_Small(_VariableHeader,addr,size_in_bytes);  }
#define transition_FLP_VariableHeader_Large(addr,size_in_bytes)  \
  {   size_t __flp_bytes_consumed = 0; \
    base_transition_FLP_Large(_VariableHeader,addr,size_in_bytes);  }
#define transition_FLP_FixedHeader(addr)  \
  {   size_t __flp_bytes_consumed = 0; \
    base_transition_FLP_FixedHeader(,addr);  }
#define transition_FLP_FixedHeader_Mini(addr)  \
  {   size_t __flp_bytes_consumed = 0; \
    base_transition_FLP_Mini(_FixedHeader,addr);  }
#define transition_FLP_FixedHeader_Big(addr)  \
  {   size_t __flp_bytes_consumed = 0; \
    base_transition_FLP_Big(_FixedHeader,addr);  }