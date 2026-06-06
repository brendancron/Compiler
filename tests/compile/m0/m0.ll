; ModuleID = 'cronyx'
source_filename = "cronyx"
target triple = "x86_64-apple-darwin23.6.0"

%__slice = type { i64, i64, ptr }

@fmt_int = private constant [6 x i8] c"%lld\0A\00"
@fmt_str = private constant [4 x i8] c"%s\0A\00"
@fmt_int_bare = private constant [5 x i8] c"%lld\00"
@.str.0 = private constant [1 x i8] zeroinitializer
@.str.1 = private constant [2 x i8] c"\0A\00"

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)

declare void @free(ptr)

declare ptr @realloc(ptr, i64)

declare i64 @strlen(ptr)

declare ptr @strcpy(ptr, ptr)

declare ptr @strcat(ptr, ptr)

declare i32 @strcmp(ptr, ptr)

declare ptr @strstr(ptr, ptr)

declare i32 @sprintf(ptr, ptr, ...)

declare ptr @memcpy(ptr, ptr, i64)

declare i64 @atoll(ptr)

declare void @abort()

define private ptr @__cronyx_trim(ptr %0) {
entry:
  %slen = call i64 @strlen(ptr %0)
  %start = alloca i64, align 8
  store i64 0, ptr %start, align 4
  br label %fwd

fwd:                                              ; preds = %fwd_inc, %entry
  %fwd_i = load i64, ptr %start, align 4
  %fwd_cmp = icmp slt i64 %fwd_i, %slen
  br i1 %fwd_cmp, label %fwd_chk, label %rev_ini

fwd_chk:                                          ; preds = %fwd
  %fwd_ptr = getelementptr i8, ptr %0, i64 %fwd_i
  %fwd_c = load i8, ptr %fwd_ptr, align 1
  %sp = icmp eq i8 %fwd_c, 32
  %tab = icmp eq i8 %fwd_c, 9
  %cr = icmp eq i8 %fwd_c, 13
  %nl = icmp eq i8 %fwd_c, 10
  %ws1 = or i1 %sp, %tab
  %ws2 = or i1 %ws1, %cr
  %ws3 = or i1 %ws2, %nl
  br i1 %ws3, label %fwd_inc, label %rev_ini

fwd_inc:                                          ; preds = %fwd_chk
  %fwd_next = add i64 %fwd_i, 1
  store i64 %fwd_next, ptr %start, align 4
  br label %fwd

rev_ini:                                          ; preds = %fwd_chk, %fwd
  %start_val = load i64, ptr %start, align 4
  %end_slot = alloca i64, align 8
  %last = sub i64 %slen, 1
  store i64 %last, ptr %end_slot, align 4
  br label %rev

rev:                                              ; preds = %rev_dec, %rev_ini
  %rev_e = load i64, ptr %end_slot, align 4
  %rev_cmp = icmp sge i64 %rev_e, %start_val
  br i1 %rev_cmp, label %rev_chk, label %empty

rev_chk:                                          ; preds = %rev
  %rev_ptr = getelementptr i8, ptr %0, i64 %rev_e
  %rev_c = load i8, ptr %rev_ptr, align 1
  %rsp = icmp eq i8 %rev_c, 32
  %rtab = icmp eq i8 %rev_c, 9
  %rcr = icmp eq i8 %rev_c, 13
  %rnl = icmp eq i8 %rev_c, 10
  %rws1 = or i1 %rsp, %rtab
  %rws2 = or i1 %rws1, %rcr
  %rws3 = or i1 %rws2, %rnl
  br i1 %rws3, label %rev_dec, label %alloc

rev_dec:                                          ; preds = %rev_chk
  %rev_dec1 = sub i64 %rev_e, 1
  store i64 %rev_dec1, ptr %end_slot, align 4
  br label %rev

alloc:                                            ; preds = %rev_chk
  %s_val = load i64, ptr %start, align 4
  %e_val = load i64, ptr %end_slot, align 4
  %new_len0 = sub i64 %e_val, %s_val
  %new_len1 = add i64 %new_len0, 1
  %buf_sz = add i64 %new_len1, 1
  %buf = call ptr @malloc(i64 %buf_sz)
  %src = getelementptr i8, ptr %0, i64 %s_val
  %mc = call ptr @memcpy(ptr %buf, ptr %src, i64 %new_len1)
  %null_pos = getelementptr i8, ptr %buf, i64 %new_len1
  store i8 0, ptr %null_pos, align 1
  ret ptr %buf

empty:                                            ; preds = %rev
  %ebuf = call ptr @malloc(i64 1)
  %enull = getelementptr i8, ptr %ebuf, i64 0
  store i8 0, ptr %enull, align 1
  ret ptr %ebuf
}

define private ptr @__cronyx_chars(ptr %0) {
entry:
  %n = call i64 @strlen(ptr %0)
  %data_sz = mul i64 %n, 8
  %data_buf = call ptr @malloc(i64 %data_sz)
  %slice_ptr = call ptr @malloc(i64 24)
  %lp = getelementptr %__slice, ptr %slice_ptr, i32 0, i32 0
  store i64 %n, ptr %lp, align 4
  %cp = getelementptr %__slice, ptr %slice_ptr, i32 0, i32 1
  store i64 %n, ptr %cp, align 4
  %dp = getelementptr %__slice, ptr %slice_ptr, i32 0, i32 2
  store ptr %data_buf, ptr %dp, align 8
  %i = alloca i64, align 8
  store i64 0, ptr %i, align 4
  br label %loop

loop:                                             ; preds = %body, %entry
  %ci = load i64, ptr %i, align 4
  %cond = icmp slt i64 %ci, %n
  br i1 %cond, label %body, label %exit

body:                                             ; preds = %loop
  %cp_i = getelementptr i8, ptr %0, i64 %ci
  %ch = load i8, ptr %cp_i, align 1
  %ch_buf = call ptr @malloc(i64 2)
  %p0 = getelementptr i8, ptr %ch_buf, i64 0
  store i8 %ch, ptr %p0, align 1
  %p1 = getelementptr i8, ptr %ch_buf, i64 1
  store i8 0, ptr %p1, align 1
  %elem_ptr = getelementptr ptr, ptr %data_buf, i64 %ci
  store ptr %ch_buf, ptr %elem_ptr, align 8
  %ci_next = add i64 %ci, 1
  store i64 %ci_next, ptr %i, align 4
  br label %loop

exit:                                             ; preds = %loop
  ret ptr %slice_ptr
}

define private ptr @__cronyx_split(ptr %0, ptr %1) {
entry:
  %dlen = call i64 @strlen(ptr %1)
  %cnt1 = alloca i64, align 8
  store i64 0, ptr %cnt1, align 4
  %pos = alloca ptr, align 8
  store ptr %0, ptr %pos, align 8
  br label %cnt

cnt:                                              ; preds = %cnt_b, %entry
  %cur = load ptr, ptr %pos, align 8
  %found = call ptr @strstr(ptr %cur, ptr %1)
  %fi = ptrtoint ptr %found to i64
  %nn = icmp ne i64 %fi, 0
  br i1 %nn, label %cnt_b, label %alloc

cnt_b:                                            ; preds = %cnt
  %oc = load i64, ptr %cnt1, align 4
  %nc = add i64 %oc, 1
  store i64 %nc, ptr %cnt1, align 4
  %next_pos = getelementptr i8, ptr %found, i64 %dlen
  store ptr %next_pos, ptr %pos, align 8
  br label %cnt

alloc:                                            ; preds = %cnt
  %cnt2 = load i64, ptr %cnt1, align 4
  %n_parts = add i64 %cnt2, 1
  %dsz = mul i64 %n_parts, 8
  %dbuf = call ptr @malloc(i64 %dsz)
  %sptr = call ptr @malloc(i64 24)
  %slp = getelementptr %__slice, ptr %sptr, i32 0, i32 0
  store i64 %n_parts, ptr %slp, align 4
  %scp = getelementptr %__slice, ptr %sptr, i32 0, i32 1
  store i64 %n_parts, ptr %scp, align 4
  %sdp = getelementptr %__slice, ptr %sptr, i32 0, i32 2
  store ptr %dbuf, ptr %sdp, align 8
  %idx = alloca i64, align 8
  store i64 0, ptr %idx, align 4
  store ptr %0, ptr %pos, align 8
  br label %fill

fill:                                             ; preds = %fill_b, %alloc
  %fp = load ptr, ptr %pos, align 8
  %fnxt = call ptr @strstr(ptr %fp, ptr %1)
  %fi2 = ptrtoint ptr %fnxt to i64
  %fnn = icmp ne i64 %fi2, 0
  br i1 %fnn, label %fill_b, label %last

fill_b:                                           ; preds = %fill
  %fpi = ptrtoint ptr %fp to i64
  %fni = ptrtoint ptr %fnxt to i64
  %slen = sub i64 %fni, %fpi
  %ssz = add i64 %slen, 1
  %sbuf = call ptr @malloc(i64 %ssz)
  %mcp = call ptr @memcpy(ptr %sbuf, ptr %fp, i64 %slen)
  %np = getelementptr i8, ptr %sbuf, i64 %slen
  store i8 0, ptr %np, align 1
  %fi3 = load i64, ptr %idx, align 4
  %ep = getelementptr ptr, ptr %dbuf, i64 %fi3
  store ptr %sbuf, ptr %ep, align 8
  %fin = add i64 %fi3, 1
  store i64 %fin, ptr %idx, align 4
  %nxtp = getelementptr i8, ptr %fnxt, i64 %dlen
  store ptr %nxtp, ptr %pos, align 8
  br label %fill

fill_e:                                           ; No predecessors!
  br label %done

last:                                             ; preds = %fill
  %lp2 = load ptr, ptr %pos, align 8
  %rem = call i64 @strlen(ptr %lp2)
  %remsz = add i64 %rem, 1
  %rbuf = call ptr @malloc(i64 %remsz)
  %rmcp = call ptr @memcpy(ptr %rbuf, ptr %lp2, i64 %rem)
  %rnull = getelementptr i8, ptr %rbuf, i64 %rem
  store i8 0, ptr %rnull, align 1
  %ri = load i64, ptr %idx, align 4
  %rep = getelementptr ptr, ptr %dbuf, i64 %ri
  store ptr %rbuf, ptr %rep, align 8
  br label %done

done:                                             ; preds = %last, %fill_e
  ret ptr %sptr
}

define i32 @main() {
entry:
  %printf_ret = call i32 (ptr, ...) @printf(ptr @fmt_int, i64 3)
  ret i32 0
}
