# https://github.com/JuliaLang/julia/pull/38236
@eval Base begin
# this function assumes `val` is found in `q`
function list_deletefirst!(q::InvasiveLinkedList{T}, val::T) where T
    val.queue === q || return
    head = q.head::T
    if head === val
        if q.tail::T === val
            q.head = q.tail = nothing
        else
            q.head = val.next::T
        end
    else
        head_next = head.next::T
        while head_next !== val
            head = head_next
            head_next = head.next::T
        end
        if q.tail::T === val
            head.next = nothing
            q.tail = head
        else
            head.next = val.next::T
        end
    end
    val.next = nothing
    val.queue = nothing
    return q
end
end # @eval Base begin
