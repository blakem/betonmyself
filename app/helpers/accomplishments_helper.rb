module AccomplishmentsHelper
  include BetHelper
  def render_descr_count(count)
    plural = count == 1 ? '' : 's'
    count == 0 ? '' : '<b>' + count.to_s + ' Completed Accomplishment' + plural + '!</b>'
  end
end
