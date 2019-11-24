
#ifndef GENERAL_H
#define GENERAL_H


#include <stdlib.h>

#include <stdint.h>
typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t  s8;

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

#include <assert.h>

#include <string.h>

#ifdef ENV64
typedef s64 string_length_type;
#else
typedef s32 string_length_type;
#endif

const int BYTES_TO_BITS = 8;

struct String {
    char *data = nullptr;
    string_length_type length = 0;
    
    String() { data = nullptr; }
    
    // String(char *str) {
    //     this->data = str;
    //     this->length = strlen(str)
    // }
    
    char operator[](string_length_type index) const {
        assert(index >= 0 && index < length);
        
        return data[index];
    }
    
    String substring(string_length_type start, string_length_type slen) {
        assert(start < length && start+slen <= length);
        
        String s;
        s.data = data + start;
        s.length = slen;
        return  s;
    }
};

inline void advance(String *s, s64 amount = 1) {
    if (s->length) {
        s->data += amount;
        s->length -= amount;
    }
}

inline String to_string(char *c_string) {
    String s;
    s.data = c_string;
    s.length = strlen(c_string);
    return s;
}

inline char *to_c_string(String s) {
    auto length = s.length;
    
    char *mem = (char *)malloc(length + 1);
    memcpy(mem, s.data, length);
    mem[s.length] = 0;
    return mem;
}

inline bool operator==(const String &s, const String &t) {
    if (s.length != t.length) return false;
    if (s.length == 0 && t.length == 0) return true;

    if (s.data == nullptr && t.data != nullptr) return false;
    if (t.data == nullptr && s.data != nullptr) return false;
    if (s.data == nullptr && t.data == nullptr) return true;
    
    for (string_length_type i = 0; i < s.length; ++i) {
        if (s[i] != t[i]) return false;
    }
    
    return true;
}

inline bool operator!=(const String &s, const String &t) {
    return !(s == t);
}

inline String copy_string(String s) {
    String out;
    out.length = s.length;
    
    auto length = s.length;
    if (s.data && s.length) {
        out.data = (char *)malloc(length);
        memcpy(out.data, s.data, length);
    }
    return out;
}

inline String basepath(String s) {
    // skip trailing slashes
    while (s.length && (s[s.length-1] == '/' || s[s.length-1] == '\\')) {
        s.length--;
    }
    while (s.length) {
        if (s[s.length-1] == '/' || s[s.length-1] == '\\') return s;
        
        s.length--;
    }
    
    return s;
}

inline String basename(String s) {
    auto length = s.length;

    // skip trailing slashes
    string_length_type skip = 0;
    while (length && (s[length-1] == '/' || s[length-1] == '\\')) {
        length--;
        skip++;
    }

    while (length) {
        if (s[length-1] == '/' || s[length-1] == '\\') {
            String out;
            out.data   = s.data + length;
            out.length = s.length - (length + skip); 
            return out;
        }
        
        length--;
    }
    
    return s;
}

String mprintf(char *fmt, ...);

inline void convert_to_back_slashes(char *c) {
    while (*c) {
        if (*c == '/') {
            *c = '\\';
        }
        
        ++c;
    }
}

inline void convert_to_forward_slashes(char *c) {
    while (*c) {
        if (*c == '\\') {
            *c = '/';
        }
        
        ++c;
    }
}


struct Span {
    string_length_type start;
    string_length_type length;
    
    Span(string_length_type start = 0, string_length_type length = 0) {
        assert(start >= 0);
        assert(length >= 0);
        
        this->start = start;
        this->length = length;
    }
    
    bool fits_in_string(String text) {
        return (!(text.length < start || text.length < start + length));
    }
    
    void map_to_text_coordinates(String text, string_length_type *line_start, string_length_type *char_start, string_length_type *line_end, string_length_type *char_end) {
        assert(fits_in_string(text));
        
        string_length_type line_count = 1;
        string_length_type char_count = 1;
        for (string_length_type i = 0; i < text.length; ++i) {
            if (i == start) {
                *line_start = line_count;
                *char_start = char_count;
            } else if (i == start+length) {
                *line_end = line_count;
                *char_end = char_count;
                return;
            }
            
            if (text[i] == '\n') {
                line_count++;
                char_count = 1;
                continue;
            }
            
            char_count++;
        }
    }
    
    void get_surrounding_lines(String text, int num_surrounding_lines, string_length_type *new_start, string_length_type *new_end, string_length_type *return_num_lines) {
        // Get the current line(s) that this span occupies
        string_length_type line_start;
        string_length_type char_start;
        string_length_type line_end;
        string_length_type char_end;
        
        map_to_text_coordinates(text, &line_start, &char_start, &line_end, &char_end);
        
        string_length_type start_line = (line_start - num_surrounding_lines);
        if (start_line < 1) start_line = 1;
        
        string_length_type end_line = (line_start + num_surrounding_lines) + 1; // Add one so we rollover to the start of the next line so that we capture all the text from the end line.
        
        string_length_type line_count  = 1;
        string_length_type start_index = -1;
        string_length_type end_index   = -1;
        for (string_length_type i = 0; i < text.length; ++i) {
            if (line_count == start_line && start_index < 0) {
                start_index = i;
            } else if (line_count == end_line && end_index < 0) {
                end_index = i;
                break;
            }
            
            if (text[i] == '\n') {
                line_count++;
                continue;
            }
        }
        
        if (start_index < 0) start_index = 0;
        *new_start = start_index;
        
        if (end_index < 0) {
            end_line  = line_count + 1;
            end_index = text.length;
        }
        *new_end   = end_index;
        
        *return_num_lines = end_line - start_line;
    }
};

struct TextSpan {
    Span span;
    String string;
    
    TextSpan() {
    }
    
    TextSpan(String string, Span span) {
        this->string = string;
        this->span = span;
        
        assert(span.fits_in_string(string));
    }

    void calculate_text_coordinates(string_length_type *line_start, string_length_type *char_start, string_length_type *line_end, string_length_type *char_end) {
        span.map_to_text_coordinates(string, line_start, char_start, line_end, char_end);
    }
    
    String get_text();
};

#ifdef ENV64
typedef s64 array_count_type;
#else
typedef s32 array_count_type;
#endif

template<typename T>
struct Array {
    T *data = nullptr;
    array_count_type count = 0;
    array_count_type allocated = 0;
    
    const int NEW_MEM_CHUNK_ELEMENT_COUNT =  16;
    
    Array(array_count_type reserve_amount = 0) {
        reserve(reserve_amount);
    }
    
    ~Array() {
        reset();
    }
    
    void reserve(array_count_type amount) {
        if (amount <= 0) amount = NEW_MEM_CHUNK_ELEMENT_COUNT;
        if (amount <= allocated) return;
        
        T *new_mem = (T *)malloc(amount * sizeof(T));
        
        if (data) {
            memcpy(new_mem, data, count * sizeof(T));
            free(data);
        }
        
        data = new_mem;
        allocated = amount;
    }
    
    void resize(array_count_type amount) {
        auto old_count = count;
        reserve(amount);
        count = amount;

        memset((char *)data + (old_count * sizeof(T)), 0, (count - old_count) * sizeof(T));
        // @TODO maybe default initalized all elements
        // that we grew by?
    }
    
    void insert(array_count_type index, T element) {
        auto old_count = count;
        resize(count + 1);
        memcpy((char *)data + ((index+1) * sizeof(T)), (char *)data + (index * sizeof(T)), old_count-index);

        data[index] = element;
    }

    void add(T element) {
        if (count+1 >= allocated) reserve(allocated + NEW_MEM_CHUNK_ELEMENT_COUNT);
        
        data[count] = element;
        count += 1;
    }
    
    T unordered_remove(array_count_type index) {
        assert(index >= 0 && index < count);
        assert(count);
        
        T last = pop();
        // if index is still within the valid range (index was not referencing the last item)
        // we put the last item in the slot we're removing.
        if (index < count) {
            (*this)[index] = last;
        }
        
        return last;
    }

    T ordered_remove(array_count_type index) {
        assert(index >= 0 && index < count);
        assert(count);

        T item = (*this)[index];
        memmove(data + index, data + index + 1, ((count - index) - 1) * sizeof(T));

        count--;
        return item;
    }
    
    T pop() {
        assert(count > 0);
        T result = data[count-1];
        count -= 1;
        return result;
    }
    
    void clear() {
        count = 0;
    }
    
    void reset() {
        count = 0;
        allocated = 0;
        
        if (data) free(data);
        data = nullptr;
    }
    
    T &operator[] (array_count_type index) {
        assert(index >= 0 && index < count);
        return data[index];
    }
    
    T *begin() {
        return &data[0];
    }
    
    T *end() {
        return &data[count];
    }
};

template<typename A, typename B>
struct Tuple {
    A item1;
    B item2;
};

template<typename A, typename B>
Tuple<A, B> MakeTuple(A a, B b) {
    Tuple<A, B> t;
    t.item1 = a;
    t.item2 = b;
    return t;
}

#include <cstdio>
#include <cstdarg>

struct String_Builder {
    
    const int BUCKET_ALLOC_SIZE = 4096;
    
    struct Bucket {
        u8 *data = nullptr;
        array_count_type count     = 0;
        array_count_type allocated = 0;
    };
    
    Array<Bucket> buckets;
    
    String_Builder() {
        make_bucket(BUCKET_ALLOC_SIZE);
    }
    
    ~String_Builder() {
        for (auto &bucket : buckets) {
            if (bucket.data) free(bucket.data);
            bucket.data = nullptr;
        }
        
        buckets.reset();
    }
    
    void make_bucket(array_count_type amount) {
        Bucket b;
        b.data = (u8 *)malloc(amount);
        b.allocated = amount;
        b.count = 0;
        buckets.add(b);
    }
    
    void putchar(char c) {
        auto bucket = &buckets[buckets.count-1];
        
        if (bucket->count < bucket->allocated) {
            bucket->data[bucket->count] = c;
            bucket->count++;
        } else {
            make_bucket(BUCKET_ALLOC_SIZE);
            putchar(c);
        }
    }
    
    void append(String s) {
        for (string_length_type i = 0; i < s.length; ++i) {
            putchar(s[i]);
        }
    }
    
    void append(char *s) {
        String o;
        o.data = s;
        o.length = strlen(s);
        append(o);
    }
    
    void print_valist(char *c_fmt, va_list vl) {
        va_list vl_copy;
        va_copy(vl_copy, vl);
        
        auto bucket = &buckets[buckets.count-1];
        auto remaining = bucket->allocated - bucket->count;
        auto written = vsnprintf((char *)bucket->data + bucket->count, remaining, c_fmt, vl);
        
        if (written < 0) return; // encoding error, @TODO maybe assert here?
        
        if (written < remaining) {
            // success
            bucket->count += written;
            assert(bucket->count <= bucket->allocated);
        } else {
            u8 *data = (u8 *)malloc(written + 1);
            auto final = vsnprintf((char *)data, written+1, c_fmt, vl_copy);
            
            assert(final >= 0);
            assert(final < written + 1);
            
            Bucket b;
            b.data = data;
            b.count = final;
            b.allocated = written+1;
            buckets.add(b);
        }
    }
    
    void print(char *c_fmt, ...) {
        va_list vl;
        va_start(vl, c_fmt);
        print_valist(c_fmt, vl);
        va_end(vl);
    }
    
    String to_string() {
        array_count_type total_data = 0;
        for (Bucket &b : buckets) {
            total_data += b.count;
        }
        
        char *data = (char *)malloc(total_data);
        array_count_type cursor = 0;
        for (Bucket &b : buckets) {
            memcpy(data+cursor, b.data, b.count);
            cursor += b.count;
        }
        
        assert(cursor == total_data);
        
        String s;
        s.data = data;
        s.length = total_data;
        return s;
    }
};

struct Pool {
    struct Chunk {
        void *data = nullptr;
        s64 used = 0;
        s64 allocated = 0;
    };
    
    const array_count_type DEFAULT_NEW_CHUNK_SIZE = 4096 * 4;
    Array<Chunk> chunks;

    ~Pool() {
        reset();
    }

    void *allocate(array_count_type amount) {
        for (auto &chunk : chunks) {
            auto available = chunk.allocated - chunk.used;
            assert(available >= 0);

            if (available >= amount) {
                void *result = (char *)chunk.data + chunk.used;
                chunk.used += amount;

                return result;
            }
        }

        // All chunks exhauted or amount simply doesnt fit..
        auto ammount_to_allocate = DEFAULT_NEW_CHUNK_SIZE;
        if (ammount_to_allocate < amount) ammount_to_allocate = amount;

        Chunk c;
        c.data = malloc(ammount_to_allocate);
        c.used = amount;
        c.allocated = ammount_to_allocate;
        chunks.add(c);

        return c.data;
    }

    void reset() {
        for (auto &chunk : chunks) {
            free(chunk.data);
        }

        chunks.reset();
    }
};

#endif
