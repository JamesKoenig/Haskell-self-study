-- in chapter 16 there's a 'lifting exercise' (fmap, signatures, composition)
--  the 5th exercise results in an IO integer with a value of 3693

e = readIO "3693"

-- now if you do this in prelude you get:
-- Prelude> e >>= print
-- 3693
--
-- but if you do it from loading the file you get:
--    lifting_exercise_addenda.hs:11:1: error:
--    Parse error: module header, import declaration
--    or top-level declaration expected.
--   |
--11 | e >>= print
--   |
--Failed, no modules loaded.

-- why?
